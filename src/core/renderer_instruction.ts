import { Pen } from "../other/pen.js";
import { Vec2Like } from "../vec/vec2.js";
import { BoundingBoxLike } from "../other/bounding_box.js";

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

export type RenderingInfo = {
  instructions: RendererInstruction[]
}
