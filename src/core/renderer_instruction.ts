import { Pen } from "../other/pen.js";

type BaseInstruction = {
  zIndex: number
}

export type PolylineRendererInstruction = BaseInstruction & {
  insnType: "polyline",
  // Flattened array of vertices; typed array is preferred
  vertices: Float32Array | Float64Array | number[],
  pen: Pen
}

export type DebugInstruction = BaseInstruction & {
  insnType: "debug",
  // Flattened array of vertices; typed array is preferred
}

export type RendererInstruction = PolylineRendererInstruction

export type RenderingInfo = {
  instructions: RendererInstruction[]
}
