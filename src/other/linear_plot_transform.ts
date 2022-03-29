import { Vec2 } from '../vec/vec2.js';

export class LinearPlot2DTransform {
  px1: number
  py1: number
  pw: number
  ph: number
  gx1: number
  gy1: number
  gw: number
  gh: number

  /**
   * Parameters beginning with p are the bounding box in pixel coordinates. Those beginning with g are the bounding box
   * in graph coordinates. The transform has an implicit y flipping operation, which is key. The point (px1, py1) does
   * NOT map to the point (gx1, gy1), but the point (gx1, gy1 + gh). This annoyance is why a special class is useful.
   */
  constructor (px1, py1, pw, ph, gx1, gy1, gw, gh) {
    this.px1 = px1
    this.py1 = py1
    this.pw = pw
    this.ph = ph
    this.gx1 = gx1
    this.gy1 = gy1
    this.gw = gw
    this.gh = gh
  }

  get px2 () {
    return this.px1 + this.pw
  }

  get py2 () {
    return this.py1 + this.ph
  }

  get gx2 () {
    return this.gx1 + this.gw
  }

  get gy2 () {
    return this.gy1 + this.gh
  }

  pixelBox () {
    return new BoundingBox(this.px1, this.py1, this.pw, this.ph)
  }

  graphBox () {
    return new BoundingBox(this.gx1, this.gy1, this.gw, this.gh)
  }

  resizeToPixelBox (box) {
    this.px1 = box.x
    this.py1 = box.y
    this.pw = box.w
    this.ph = box.h

    return this
  }

  resizeToGraphBox (box) {
    this.gx1 = box.x
    this.gy1 = box.y
    this.gw = box.w
    this.gh = box.h

    return this
  }

  setGraphXBounds (x1, x2) {
    this.gx1 = x1
    this.gw = x2 - x1
  }

  setGraphYBounds (y1, y2) {
    this.gy1 = y1
    this.gh = y2 - y1
  }

  setGraphXYBounds (x1, x2, y1, y2) {
    this.setGraphXBounds(x1, x2)
    this.setGraphYBounds(y1, y2)
  }

  clone () {
    return new LinearPlot2DTransform(
      this.px1,
      this.py1,
      this.pw,
      this.ph,
      this.gx1,
      this.gy1,
      this.gw,
      this.gh
    )
  }

  pixelToGraphX (x) {
    // This is not flipped
    return ((x - this.px1) / this.pw) * this.gw + this.gx1
  }

  pixelToGraphY (y) {
    // This is flipped
    return (1 - (y - this.py1) / this.ph) * this.gh + this.gy1
  }

  graphToPixelX (x) {
    // This is not flipped
    return ((x - this.gx1) / this.gw) * this.pw + this.px1
  }

  graphToPixelY (y) {
    // This is flipped
    return (1 - (y - this.gy1) / this.gh) * this.ph + this.py1
  }

  pixelToGraph (vec) {
    return new Vec2(this.pixelToGraphX(vec.x), this.pixelToGraphY(vec.y))
  }

  graphToPixel (vec) {
    return new Vec2(this.graphToPixelX(vec.x), this.graphToPixelY(vec.y))
  }

  /**
   * Return {xm, ym, xb, yb} where xm * x + xb is the transformation from graph x to pixel x, etc.
   */
  getReducedGraphToPixelTransform () {
    const { px1, py1, pw, ph, gx1, gy1, gw, gh } = this

    return {
      xm: pw / gw,
      ym: -ph / gh,
      xb: (-gx1 / gw) * pw + px1,
      yb: (1 + gy1 / gh) * ph + py1
    }
  }

  graphToPixelArrInPlace (arr) {
    let { xm, ym, xb, yb } = this.getReducedGraphToPixelTransform()

    for (let i = 0; i < arr.length; i += 2) {
      arr[i] = xm * arr[i] + xb
      arr[i+1] = ym * arr[i+1] + yb
    }

    return arr
  }

  /**
   * The size, in graph units, of a single pixel
   */
  graphPixelSize () {
    return this.gh / this.ph
  }
}
