/**
 * Here lies madness.
 *
 * Grapheme's renderer is going to be pretty monolithic, with a lot of interdependent moving parts. As such, I'm going
 * to keep it mostly contained within one class, perhaps with some helper classes. Doing so will also help eliminate
 * fluff and make optimization easy and expressive. In any case, the renderer is effectively a state machine of various
 * draw calls. The final call to copy the internal buffer to the screen uses transferFromImageBitmap, which is
 * asynchronous and thus allows WebGL to proceed in a a separate thread—as it is implemented by modern browsers.
 *
 * On the surface, Grapheme's rendering sequence is simple: the renderer traverses through the scene, calls
 * getRenderingInfo() on every element, compiles a list of all the instructions (which look something like
 * "draw this set of triangles", "draw this text"), and runs them all, returning the final product. But if the rendering
 * pipeline were so simple, there would be little point in using WebGL at all. Why not just use Canvas2D? Why learn such
 * a painful API? The name of the game is parallelism and optimization. Where WebGL excels is low-level control
 * and rapid parallel computation. Its weaknesses are in a lack of builtin functions (lacking text, for example) and
 * high complexity and verbosity,
 *
 * Imagine we did indeed render a scene instruction by instruction. We come across a line, so we switch to the polyline
 * program, load in the vertices into a buffer, and drawArrays -- draw it to the canvas. We then come across a piece of
 * text. WebGL cannot render text, so we switch over to a Canvas2D context and draw a piece of text onto a blank canvas.
 * We then load the blank canvas as a texture into WebGL and switch to the text program, loading in a set of vertices
 * specifying where the text is, and calling drawArrays. We then come across a couple hundred polylines in a row. For
 * each polyline, we copy its data to the buffer and render it.
 *
 * There are two serious problems here. One is that loading buffers and textures is slow, for various
 * reasons. Another is that parallelism is seriously lacking. We have to call drawArrays several hundred times for those
 * polylines, and each call has a large constant overhead.
 *
 * The renderer thus has several difficult jobs: minimizing buffer and texture loading, and combining consecutive calls
 * into one large drawArrays call. Accomplishing these jobs (and a few more) requires somewhat intricate work,
 * which should of course be designed to allow more esoteric draw calls -- for a Mandelbrot set, say -- to still be
 * handled with consistency. There is no perfect solution, but there are certainly gains to be made. As with the props
 * of Grapheme elements, the problem is made easier by high-level abstraction. The renderer should produce a comparable
 * result when optimized, compared to when every call is made individually. (They need not be exactly the same, for
 * reasons that will become apparent.)
 *
 * Even more annoying is that the WebGL context may suddenly crash and all its buffers and programs lost in the ether.
 * The renderer thus has to be able to handle such data loss without indefinitely screwing up the rendering process. So
 * I have my work cut out, but that's exciting.
 *
 * The current thinking is a z-index based system with heuristic reallocation of changing and unchanging buffers. Given
 * a list of elements and each element's instructions, we are allowed to rearrange the instructions under certain
 * conditions: 1. instructions are drawn in order of z-index and 2. specific instructions within a given z-index may
 * specify that they must be rendered in the order in which they appear in the instruction list. The latter condition
 * allows deterministic ordering of certain instructions on the same z-index, which is useful when that suborder does
 * matter (like when two instructions for a given element are intended to be one on top of the other). Otherwise, the
 * instructions may be freely rearranged and (importantly) combined into larger operations that look the same.
 *
 * Already, such a sorting system is very helpful. Text elements generally specify a z-index of Infinity, while
 * gridlines might specify a z-index of 0 to be behind most things, and a draggable point might have an index of 20. A
 * simple algorithm to render a static image is to sort by z-index, then within each z-index group triangle draw calls
 * with the same color together, and group text draw calls together. We then proceed to render each z-index's grouped
 * calls in order.
 *
 * For a static scene, such a rendering system would work great. But in a dynamic scene, constantly reoptimizing the
 * entire scene as a result of changing some inconsequential little geometry would be stupid. Ideally, changing a little
 * geometry would merely update a single buffer or subsection of a buffer. Yet some changes do require a complete re-
 * distribution of instructions; if the scene's size doubled, for example, and all the elements changed substantially.
 * We can certainly cache information from the previous rendering process of a scene, but what do we cache? How do we
 * ensure stability and few edge cases? How do we deal with context loss?
 *
 * The first step is to understand exactly what instructions are. *Anonymous* instructions have a type, some data, and
 * an element id (which element it originated from). *Normal* instructions have a type, some data, an element id, an
 * instruction id, and a version. The point of normal instructions is to represent a sort of "draw concept", where after
 * an update, that instruction may have changed slightly, but will still have the same id. The instruction associated
 * with a function plot, for example, will have some numerical ID, and when the plot changes somehow, the version will
 * increase, but the numerical ID will remain the same. Conceptually, this means that the instruction to draw the
 * function plot has been rewritten, and the old data is basically irrelevant -- and buffers associated with that
 * data can and should be reused or reallocated.
 *
 * Anonymous instructions, on the other hand, have no concept of "versioning". Anonymous instructions are
 * entirely reallocated or deleted every time their element updates. These instructions are generally used to indicate
 * instructions which are very prone to change and where its values should be tied solely to the element updating.
 */

import { TextRenderer } from './text_renderer.js'
import { SceneGraph } from './scene_graph.js'
import { Color, Colors } from '../other/color.js'
import { calculateRectShift } from '../other/text_utils.js'
import { BoundingBox } from '../other/bounding_box.js'
import { Vec2 } from "../vec/vec2.js";

type ShaderType = number

// Functions taken from Mozilla docs
function createShaderFromSource (gl: WebGL2RenderingContext, shaderType: ShaderType, shaderSource: string): WebGLShader {
  const shader = gl.createShader(shaderType)
  let err

  if (!shader) {
    err = "Failed to create shader"
  } else {
    gl.shaderSource(shader, shaderSource)
    gl.compileShader(shader)

    const succeeded = gl.getShaderParameter(shader, gl.COMPILE_STATUS)

    if (succeeded) return shader
    err = (gl.getShaderInfoLog(shader) ?? "Failed to create shader or get info log")
  }

  gl.deleteShader(shader)
  throw new Error("createShaderFromSource: " + err)
}

function createGLProgram (gl: WebGL2RenderingContext, vertexShader: WebGLShader, fragShader: WebGLShader): WebGLProgram {
  const program = gl.createProgram()
  let err
  if (!program) {
    err = "Failed to create program"
  } else {
    gl.attachShader(program, vertexShader)
    gl.attachShader(program, fragShader)

    gl.linkProgram(program)

    const succeeded = gl.getProgramParameter(program, gl.LINK_STATUS)

    if (succeeded) return program
    err = gl.getProgramInfoLog(program) ?? "Failed to create program or get info log-"
  }

  gl.deleteProgram(program)
  throw new Error("createGLProgram: " + err)
}

type RendererInstruction = {

}

type GLProgramAttributes = {
  // Dictionary between attribute name and location
  [key: string]: number
}

type GLProgramUniforms = {
  [key: string]: WebGLUniformLocation
}

type GLProgramUniformsList = string[]

type GLProgramStore = {
  program: WebGLProgram
  attributes: GLProgramAttributes
  uniforms: GLProgramUniforms
}

type GLBufferStore = {
  buffer: WebGLBuffer
}

type GLTextureStore = {
  texture: WebGLTexture
}

type GLVertexArrayObjectStore = {
  vao: WebGLVertexArrayObject
}

export class WebGLRenderer {
  // Main rendering buffer
  canvas: HTMLCanvasElement

  // GL context
  gl: WebGL2RenderingContext

  // Pixel ratio of the renderer (which may be different from the device; the pixel ratio is treated somewhat abstractly)
  dpr: number

  sceneCaches: Map<string, any>
  programs: Map<string, GLProgramStore>
  buffers: Map<string, GLBufferStore>
  textures: Map<string, GLTextureStore>
  vaos: Map<string, GLVertexArrayObjectStore>

  constructor () {
    const canvas = document.createElement('canvas')
    const gl = canvas.getContext('webgl2')

    if (!gl) {
      throw new Error("WebGL2 not supported")
    }

    this.canvas = canvas
    this.gl = gl

    /**
     * Map between scene ids and known information about them
     * @type {Map<string, {}>}
     */
    this.sceneCaches = new Map()

    this.programs = new Map()

    this.buffers = new Map()

    this.textures = new Map()

    this.vaos = new Map()
  }

  /**
   * Create and link a program and store it in the form { glProgram, attribs, uniforms }, where glProgram is the
   * underlying program and attributes and uniforms are a dictionary of attributes and uniforms from the program. The
   * attributes are given as an object, of manually assigned indices; these are required and will not be assigned
   * automatically. Returns null on failure—does not throw
   * @param programName
   * @param vertexShaderSource
   * @param fragShaderSource
   * @param attributeBindings
   * @param uniformNames
   * @return GLProgramStore
   */
  createProgram (programName: string, vertexShaderSource: string, fragShaderSource: string,
                 attributeBindings: GLProgramAttributes, uniformNames: GLProgramUniformsList): GLProgramStore | null {
    this.deleteProgram(programName)

    const { gl } = this

    const glProgram = createGLProgram(
      gl,
      createShaderFromSource(gl, gl.VERTEX_SHADER, vertexShaderSource),
      createShaderFromSource(gl, gl.FRAGMENT_SHADER, fragShaderSource)
    )

    for (let name in attributeBindings) {
      let loc = attributeBindings[name]

      gl.bindAttribLocation(glProgram, loc, name)
    }

    const uniforms: GLProgramUniforms = {}
    for (const name of uniformNames) {
      let loc = gl.getUniformLocation(glProgram, name)

      if (!loc) {
        gl.deleteProgram(glProgram)
        throw new Error(`Unable to find uniform ${name}`)
      }

      uniforms[name] = loc
    }

    const program = { program: glProgram, attributes: attributeBindings, uniforms }
    this.programs.set(programName, program)

    return program
  }

  /**
   * Get the program of a given name, returning undefined if it does not exist
   * @param programName
   * @returns
   */
  getProgram (programName: string): GLProgramStore | null {
    return this.programs.get(programName) ?? null
  }

  /**
   * Delete a program, including the underlying GL program
   * @param programName {string}
   */
  deleteProgram (programName: string) {
    const program = this.getProgram(programName)

    if (program) {
      this.gl.deleteProgram(program.program)
      this.programs.delete(programName)
    }
  }

  getTexture (textureName: string): GLTextureStore | null {
    return this.textures.get(textureName) ?? null
  }

  deleteTexture (textureName: string) {
    let texture = this.getTexture(textureName)

    if (texture !== undefined) {
      this.gl.deleteTexture(this.getTexture(textureName))
      this.textures.delete(textureName)
    }
  }

  createTexture (textureName: string): GLTextureStore | null {
    this.deleteTexture(textureName)
    const texture = this.gl.createTexture()

    if (!texture) return null
    const store = { texture }

    this.textures.set(textureName, store)
    return store
  }

  getBuffer (bufferName: string): GLBufferStore | null {
    return this.buffers.get(bufferName) ?? null
  }

  /**
   * Create a buffer with a given name, returning the existing buffer if one exists
   * @param bufferName
   */
  createBuffer (bufferName: string): GLBufferStore | null {
    let buffer = this.getBuffer(bufferName)

    if (!buffer) {
      let glBuffer = this.gl.createBuffer()
      if (!glBuffer) return null

      buffer = { buffer: glBuffer }

      this.buffers.set(bufferName, buffer)
    }

    return buffer
  }

  deleteBuffer (bufferName: string) {
    const buffer = this.getBuffer(bufferName)

    if (buffer) {
      this.buffers.delete(bufferName)
      this.gl.deleteBuffer(buffer)
    }
  }

  getVAO (vaoName: string): GLVertexArrayObjectStore | null {
    return this.vaos.get(vaoName) ?? null
  }

  createVAO (vaoName: string): GLVertexArrayObjectStore | null {
    let vao = this.getVAO(vaoName)

    if (!vao) {
      let glVao = this.gl.createVertexArray()
      if (!glVao) return null

      this.vaos.set(vaoName, { vao: glVao })
    }

    return vao
  }

  deleteVAO (vaoName: string) {
    const vao = this.getVAO(vaoName)

    if (vao) {
      this.vaos.delete(vaoName)
      this.gl.deleteVertexArray(vao.vao)
    }
  }

  /**
   * Resize and clear the canvas simultaneously. The canvas is only manually cleared if the dimensions haven't changed,
   * since the buffer will be erased.
   * @param width
   * @param height
   * @param dpr
   * @param clear {Color}
   */
  clearAndResizeCanvas (width: number, height: number, dpr=1, clear = Colors.TRANSPARENT) {
    const { canvas } = this

    this.dpr = dpr

    if (canvas.width === width && canvas.height === height) {
      // No need to reset the canvas
      this.clearCanvas(clear)
    } else {
      canvas.width = width
      canvas.height = height

      // If the given color is not plain transparent black, we need to set the canvas color directly
      if (!clear.equals(Colors.TRANSPARENT)) {
        this.clearCanvas(clear)
      }
    }

    this.gl.viewport(0, 0, width, height)
  }

  /**
   * Clear the canvas to the given color
   * @param clearColor
   */
  clearCanvas (clearColor: Color = Colors.TRANSPARENT) {
    const gl = this.gl

    gl.clearColor(
      clearColor.r / 255,
      clearColor.g / 255,
      clearColor.b / 255,
      clearColor.a / 255
    )
    gl.clear(gl.COLOR_BUFFER_BIT)
  }

  /**
   * Overall scaling from pixel space to clip space ((-1,-1) bottom left; (1,1) top right). In other words, the distance
   * between two pixels horizontally is v.x in clip space, and vertically, v.y.
   */
  getXYScale (): Vec2 {
    let { canvas, dpr } = this

    return new Vec2(2 / canvas.width * dpr, -2 / canvas.height * dpr)
  }

  renderDOMScene (scene) {
    let graph = new SceneGraph(this)

    window.graph = graph

    graph.constructFromScene(scene)
    graph.assembleInstructions()
    graph.compile()

    createImageBitmap(this.canvas).then(bitmap => {
      console.log(bitmap)
      scene.bitmapRenderer.transferFromImageBitmap(bitmap)
    })
  }
}
