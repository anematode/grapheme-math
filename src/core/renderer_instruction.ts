import { Pen } from "../other/pen.js";
import { Vec2Like } from "../vec/vec2.js";
import { BoundingBoxLike } from "../other/bounding_box.js";
import { SceneDimensions } from "../other/scene_dims.js";
import { Color } from "../other/color.js";
import {TextStyle} from "../other/text_style.js"
import {TextRect} from "./text_renderer.js"

export class VertexData {
  vertices: null | Float32Array // null implies no data
  vertexCount: number
  dim: number
  // Assumes the vertex data is immutable. If the vertex data is changed, the version should be updated accordingly. -1
  // is unversioned.
  version?: number

  constructor (vertices: null | Float32Array, vertexCount: number, dim: number, version: number = -1) {
    this.vertices = vertices
    this.vertexCount = vertexCount
    this.dim = dim
    this.version = version
  }
}

type InstructionPrimitiveType = "triangles" | "lines" | "points" | "triangle_strip" | "line_strip"

type BaseRendererInstruction = {
  // Displayed z-index (higher is toward front, default: 0)
  zIndex?: number,
  // Level of contexts to escape when rendering (default: 0)
  escapeContext?: number
  // Number indicating whether an instruction has changed (default: -1)
  version?: number
}

export type PrimitiveRendererInstruction = BaseRendererInstruction & {
  type: "primitive"
  primitive: InstructionPrimitiveType
  vertexData: VertexData
  color: Color

  compiled?: {     // somewhat of an implementation detail...
    vaoName: string    // key to VAO
    buffName: string
  }
}

type BaseContextInstruction = {
  version?: number
}

export type SceneContextInstruction = BaseContextInstruction & {
  type: "scene"
  sceneDims: SceneDimensions
  backgroundColor: Color
}

// Sui generis instruction which indicates the end of a context. Should never be used by elements
type PopContextInstruction = BaseContextInstruction & {
  type: "pop"
}

export type TextInstruction = BaseContextInstruction & {
  type: "text",

  style: TextStyle
  text: string

  compiled?: {
    rect: TextRect
    textureObject: string
  }
}

export type RendererInstruction = PrimitiveRendererInstruction | TextInstruction
export type ContextInstruction = SceneContextInstruction | PopContextInstruction

/**
 * General form of rendering info outputted by a given element
 */
export type RenderingInfo = {
  contexts?: ContextInstruction[]
  instructions?: RendererInstruction[]
  version?: number
} | null
