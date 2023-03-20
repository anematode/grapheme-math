import {WebGLRenderer} from "./renderer.js"
import {Scene} from "./scene.js"
import {Element} from "./element.js"
import {
  ContextInstruction,
  PrimitiveRendererInstruction,
  RendererInstruction,
  RenderingInfo, TextInstruction
} from "./renderer_instruction.js";
import {Group} from "./group.js"
import {getVersionID} from "../utils.js";
import {TEXT_RENDERER, TextInfo} from "./text_renderer.js"

class SceneGraphPerfCounters {
  insns: number
  contexts: number
  elements: number

  constructor() {
    this.reset()
  }

  reset() {
    this.insns = this.contexts = this.elements = 0
  }
}

export class SceneGraph {
  renderer: WebGLRenderer | null
  perfCounters: SceneGraphPerfCounters

  instructionList: (RendererInstruction|ContextInstruction)[]
  compiledInstructions: (RendererInstruction|ContextInstruction)[]

  // Used as a texture to draw textedText: ImageData

  static MAX_NODES = 100000

  constructor(renderer: WebGLRenderer | null | undefined) {
    this.renderer = renderer ?? null
    this.perfCounters = new SceneGraphPerfCounters()
  }

  // Whether this graph is attached to a renderer, or is "headless"
  hasRenderer(): boolean {
    return this.renderer === null
  }

  buildFromScene(scene: Scene) {
    // Procedure:
    //  - Get all scene instructions and contexts
    //  - Process zIndex and escapeContext
    //  - Flatten into single list of instructions (push/pop context, etc.)

    let perfCounters = this.perfCounters
    perfCounters.reset()

    let stack: Element[] = [ scene, scene ]
    let nodeCount = 0

    let instructionList: (RendererInstruction|ContextInstruction)[] = []

    let rInfoMap: Map<Element, RenderingInfo> = new Map()

    while (nodeCount++ < SceneGraph.MAX_NODES) {
      let node = stack.pop(), info: RenderingInfo
      if (!node) break

      let node2 = stack[stack.length - 1]
      if (node !== node2) {
        let maybeInfo = rInfoMap.get(node)
        if (maybeInfo === undefined) throw new Error("Something went wrong")
        info = maybeInfo

        if (info && info.contexts) {
          for (let i = info.contexts.length; i > 0; --i) {
            instructionList.push({ type: "pop" })
          }
        }

        stack.pop()
      } else {
        info = node.getRenderingInfo()
        rInfoMap.set(node, info)

        if (node.isGroup()) {
          let c = (node as Group).children
          for (let ch of c) {
            stack.push(ch, ch)
          }
        }

        if (info) {
          if (info.contexts) {
            for (let c of info.contexts) {
              instructionList.push(c)
              perfCounters.contexts++
            }
          }

          if (info.instructions) {
            for (let insn of info.instructions) {
              instructionList.push(insn)
              perfCounters.insns++
            }
          }
        }
      }
    }

    if (nodeCount === SceneGraph.MAX_NODES) {
      throw new Error(`Number of nodes exceeded ${SceneGraph.MAX_NODES} -- likely cycle`)
    }

    this.instructionList = instructionList
  }

  compile() {
    let insnList = this.instructionList
    this.compiledInstructions = []
    let compiled = this.compiledInstructions

    let renderer = this.renderer
    if (!renderer) throw new Error("Scene graph needs attached renderer to compile")

    let gl = renderer.gl
    let textInstructions: TextInfo[] = []

    for (let i = 0; i < insnList.length; ++i) {
      let _insn = insnList[i]
      let compiledInsn

      switch (_insn.type) {
        case "primitive": {
          let insn = _insn as PrimitiveRendererInstruction

          let buffName = "" + getVersionID()
          let vaoName = "" + getVersionID()
          let vertexData = insn.vertexData

          let vao = renderer.createVAO(vaoName)
          let buffer = renderer.createBuffer(buffName)

          if (!vao || !buffer) throw new Error("Failed to create")

          gl.bindVertexArray(vao.vao)

          gl.bindBuffer(gl.ARRAY_BUFFER, buffer.buffer)
          gl.enableVertexAttribArray(0) // position buffer
          gl.vertexAttribPointer(0, 2, gl.FLOAT, false, 0, 0)

          gl.bufferData(gl.ARRAY_BUFFER, vertexData.vertices, gl.STATIC_DRAW)

          let _compiledInsn: PrimitiveRendererInstruction = { ...insn }
          _compiledInsn.compiled = {
            vaoName: vaoName,
            buffName: buffName
          }

          compiledInsn = _compiledInsn
          break
        }

        case "text": {
          let insn = _insn as TextInstruction
          let rect = { w: 0, h: 0, x: 0, y: 0 }

          textInstructions.push({
            style: insn.style,
            text: insn.text,
            rect: rect
          })

          let _compiledInsn = { ...insn }
          _compiledInsn.compiled = {
            rect: rect,     // to be calculated
            textureObject: this.textObject
          }
          break
        }

        case "scene":
        case "pop": {
          compiledInsn = _insn
          break
        }
      }

      compiled.push(compiledInsn)
    }

    // Render text
    TEXT_RENDERER.drawText()
    this.compiledInstructions = compiled
  }

}