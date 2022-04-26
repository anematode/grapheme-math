import { Element, ElementInternalStore } from "../core/element.js";
import { Pen, PenLike } from "../other/pen.js";
import { Color, ColorLike } from "../other/color";
import { vec2ArrayConversion, Vec2ArrayLike } from "../vec/vec2";
import { BoundingBox } from "../other/bounding_box";
import { calculatePolylineVertices, PolylineTriangulationResult } from "../algorithm/polyline_triangulation";

type PolylineElementInternal = ElementInternalStore & {
  // Triangle strip
  triangulation?: PolylineTriangulationResult
}

export class PolylineElement extends Element {
  internal: PolylineElementInternal

  init (params) {

  }

  /**
   * Set the pen, merging properties. For example, setting the pen color, then the thickness, will not reset the color.
   * @param pen
   */
  setPen (pen: PenLike) {
    this.props.set("pen",
      Pen.compose(Pen.default(), pen))

    return this
  }

  private _getPen (): Pen {
    let p = this.props.get("pen")
    if (!p) this.props.set("pen", p = Pen.default())
    return p
  }

  getPen (): Pen {
    return Pen.create(this._getPen())
  }

  setColor (color: ColorLike) {
    this.props.get("pen").color = Color.fromObj(color)
    this.props.markChanged("pen")

    return this
  }

  /**
   * Set the vertices, which will be converted to a flat array. Setting to undefined will clear the vertices.
   * @param v
   * @param f32 Whether to convert to single-precision to save space
   */
  setVertices (v: Vec2ArrayLike | undefined, f32: boolean = true) {
    this.props.set("vertices",
      v ? vec2ArrayConversion(v, f32, true) : v)
  }

  /**
   * Returns a flat array of the vertices
   */
  getVertices (): Float32Array | Float64Array | undefined {
    return this.props.get("vertices")
  }

  private _computeDrawVertices () {
    let setDrawVertices = (v: undefined | PolylineTriangulationResult) => {
      this.internal.triangulation = v
    }

    let vertices = this.getVertices()
    let pen = this.getPen()

    if (!vertices || !(pen instanceof Pen)) {
      setDrawVertices(undefined)
      return
    }

    let drawVertices = calculatePolylineVertices(vertices, pen)
    setDrawVertices(drawVertices)
  }

  getBoundingBox () {

  }

  private _computeBoundingBox () {

  }

  update () {
    // The algorithm is as follows:
    // If pen and vertices have not changed, we do nothing.
    // If vertices have changed, we recompute everything.
    // If pen has changed and only the color has changed, we recompute nothing.
  }

  clone (): PolylineElement {
    let e = new PolylineElement()

    e.setPen(this._getPen())
    e.setVertices(this.getVertices())

    // TODO copy precomputed information?

    return e
  }
}
