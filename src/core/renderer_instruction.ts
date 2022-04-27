import { Pen } from "../other/pen.js";
import { Vec2Like } from "../vec/vec2.js";
import { BoundingBoxLike } from "../other/bounding_box.js";
import { SceneDimensions } from "./scene.js";
import { Color } from "../other/color.js";

type BaseContextInstruction = {

}

type SceneContextInstruction = BaseContextInstruction & {
  type: "scene"
  dims: SceneDimensions
  backgroundColor: Color
}

type BaseInstruction = {
  zIndex: number
}

export type PolylineRendererInstruction = BaseInstruction & {
  insnType: "polyline",
  // Flattened array of vertices; typed array is preferred
  vertices: Float32Array | Float64Array | number[],
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

export type RendererInstruction = PolylineRendererInstruction | DebugInstruction
export type RendererContextInstruction = SceneContextInstruction

/**
 * General form of rendering info outputted by a given element
 */
export type RenderingInfo = {
  contexts?: RendererContextInstruction[]
  instructions?: RendererInstruction[]
}
