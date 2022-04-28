import { Pen } from "../other/pen.js";
import { Vec2Like } from "../vec/vec2.js";
import { BoundingBoxLike } from "../other/bounding_box.js";
import { SceneDimensions } from "./scene.js";
import { Color } from "../other/color.js";

export type VertexData = {
  vertices: null | Float32Array, // null implies no data
  vertexCount: number,
  dim: number
}

type BaseContextInstruction = {
  zIndex?: number
}

type SceneContextInstruction = BaseContextInstruction & {
  insnType: "scene"
  dims: SceneDimensions
  backgroundColor: Color
}

type BaseInstruction = {
  zIndex?: number
}

export type PolylineRendererInstruction = BaseInstruction & {
  insnType: "polyline",
  // Flattened array of vertices; typed array is preferred
  vertices: Float32Array | Float64Array | number[],
  pen: Pen
}

export type PrimitiveRendererInstruction = BaseInstruction & {
  insnType: "primitive",
  primitiveType: "triangles" | "lines" | "points" | "triangle_strip" | "line_strip",
  vertexData: VertexData
  pen: Pen
}

type DebugType = "rectangle" | "point"

// Because it's used for debugging, we're relatively permissive about inputs for convenience
export type DebugInstruction = BaseInstruction & {
  insnType: "debug",
  type: DebugType,
  location?: Vec2Like,
  rect?: BoundingBoxLike
}

export type RendererInstruction = PolylineRendererInstruction | DebugInstruction | PrimitiveRendererInstruction
export type RendererContextInstruction = SceneContextInstruction

/**
 * General form of rendering info outputted by a given element
 */
export type RenderingInfo = {
  contexts?: RendererContextInstruction[]
  instructions?: RendererInstruction[]
}
