import { Pen } from "../other/pen.js";
import { Vec2Like } from "../vec/vec2.js";
import { BoundingBoxLike } from "../other/bounding_box.js";
import { SceneDimensions } from "../other/scene_dims.js";
import { Color } from "../other/color.js";

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

type BaseContextInstruction = {
  zIndex?: number
}

export type SceneContextInstruction = BaseContextInstruction & {
  insnType: "scene"
  dims: SceneDimensions
  backgroundColor: Color
}

type BaseInstruction = {
  zIndex?: number
  // Must be updated every time the instruction changes
  version?: number
}

export type PolylineRendererInstruction = BaseInstruction & {
  insnType: "polyline"
  // Flattened array of vertices; typed array is preferred
  vertices: Float32Array | Float64Array | number[]
  pen: Pen
}

type PrimitiveInstructionType = "triangles" | "lines" | "points" | "triangle_strip" | "line_strip"

export type PrimitiveRendererInstruction = BaseInstruction & {
  insnType: "primitive"
  primitiveType: PrimitiveInstructionType
  vertexData: VertexData
  color: Color
}

type DebugType = "rectangle" | "point"

// Because it's used for debugging, we're relatively permissive about inputs for convenience
export type DebugInstruction = BaseInstruction & {
  insnType: "debug"
  type: DebugType
  location?: Vec2Like
  rect?: BoundingBoxLike
}

// Elements use these instructions
export type RendererInstruction = PolylineRendererInstruction | DebugInstruction | PrimitiveRendererInstruction
export type RendererContextInstruction = SceneContextInstruction

type BaseCompiledRendererInstruction = {

}

export type CompiledPrimitiveRendererInstruction = BaseCompiledRendererInstruction & {
  insnType: "primitive"
  primitiveType: PrimitiveInstructionType
  vao: string  // ptr to VAO array
  buffers: string[]  // ptr to relevant buffers
  color: Color
  vertexCount: number
}

type BaseCompiledRendererContextInstruction = {

}

export type CompiledSceneContextInstruction = BaseCompiledRendererInstruction & {
  insnType: "scene"
  sceneDims: SceneDimensions
  backgroundColor: Color
}

// Good in case the previous render fucked something up
export type ContextPopAllRendererInstruction = BaseCompiledRendererInstruction & {
  insnType: "pop_all"
}
export type ContextPushRendererInstruction = CompiledSceneContextInstruction
export type ContextPopRendererInstruction = BaseCompiledRendererInstruction & {
  insnType: "pop_context"
}

export type CompiledRendererContextInstruction = ContextPopAllRendererInstruction | ContextPushRendererInstruction | ContextPopRendererInstruction

// The renderer understands these instructions
export type CompiledRendererInstruction = CompiledPrimitiveRendererInstruction | CompiledRendererContextInstruction

/**
 * General form of rendering info outputted by a given element
 */
export type RenderingInfo = {
  contexts?: RendererContextInstruction[]
  instructions?: RendererInstruction[]
  version?: number
}
