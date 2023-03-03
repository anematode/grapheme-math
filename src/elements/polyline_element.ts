import { Element, ElementInternalStore } from "../core/element.js";
import { Pen, PenLike } from "../other/pen.js";
import { Color, ColorLike } from "../other/color.js";
import { vec2ArrayConversion, Vec2ArrayLike } from "../vec/vec2.js";
import { BoundingBox } from "../other/bounding_box.js";
import { calculatePolylineVertices, PolylineTriangulationResult } from "../algorithm/polyline_triangulation.js";
import { getVersionID } from "../utils.js"

type PolylineElementInternal = ElementInternalStore & {
  // Triangle strip
  triangulation?: PolylineTriangulationResult | null
  drawPen?: Pen
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
    let setDrawVertices = (v: null | PolylineTriangulationResult) => {
      this.internal.triangulation = v
    }

    let vertices = this.getVertices()
    let pen = this.getPen()

    this.internal.drawPen = pen; // cloned

    if (!vertices || !(pen instanceof Pen)) {
      setDrawVertices(null)
      return
    }

    let drawVertices = calculatePolylineVertices(vertices, pen)
    setDrawVertices(drawVertices)
  }

  getBoundingBox () {

  }

  private _computeBoundingBox () {

  }

  _update () {
    // The algorithm is as follows:
    // If pen has changed and only the color has changed, we recompute nothing.

    let props = this.props
    let pen = this._getPen()
    let needsRecompute = true

    if (!props.hasChanged("vertices")) {
      // Vertices haven't changed, check if the pen has changed
      if (!props.hasChanged("pen")) return // nothing changed

      let previousPen = this.internal.drawPen
      if (previousPen) {
        previousPen.color = pen.color

        if (previousPen.equals(pen)) { // pens are the same except for color; vertices don't need recomputation
          needsRecompute = false
        }
      }
    }

    if (needsRecompute)
      this._computeDrawVertices()

    this.internal.drawPen = pen

    let vertexData = this.internal.triangulation
    this.internal.renderInfo = vertexData ? {
      instructions: [
        {
          type: "primitive",
          primitive: "triangle_strip",
          vertexData,
          color: pen.color,
          version: getVersionID()
        }
      ]
    } : null
  }

  clone (): PolylineElement {
    let e = new PolylineElement()

    e.setPen(this._getPen())
    e.setVertices(this.getVertices())

    // TODO copy precomputed information?

    return e
  }
}
