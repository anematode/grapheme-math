// TODO
import { Vec2 } from '../vec/vec2.js'
import { isTypedArray, NumericArray } from "../grapheme_shared.js";

/**
 * Given some parameters describing a line segment, find a line segment that is consistent with at least two of them.
 * @param x1 {number}
 * @param x2 {number}
 * @param w {number}
 * @param cx {number}
 */
function resolveAxisSpecification (x1: number | undefined, x2: number | undefined, w: number | undefined, cx: number | undefined): [ number, number ] {
  if (cx !== undefined) {
    let halfWidth = 0

    if (w !== undefined) halfWidth = w / 2
    else if (x2 !== undefined) halfWidth = x2 - cx
    else if (x1 !== undefined) halfWidth = cx - x1

    halfWidth = Math.abs(halfWidth)

    return [cx - halfWidth, cx + halfWidth]
  } else if (x1 !== undefined) {
    if (w !== undefined) return [x1, x1 + w]
    if (x2 !== undefined) return [x1, x2]
  } else if (x2 !== undefined) {
    if (w !== undefined) return [x2 - w, x2]
  }

  return [0, 0]
}

// Type that may be permissively cast into a bounding box via fromObj
export type BoundingBoxLike = {
  x?: number
  x1?: number
  y?: number
  y1?: number
  x2?: number
  y2?: number
  w?: number
  width?: number
  h?: number
  height?: number
  cx?: number
  cy?: number
  centerX?: number
  centerY?: number
} | [ number, number, number, number ] | BoundingBox

/**
 * A bounding box. In general, we consider the bounding box to be in canvas coordinates, so that the "top" is -y and
 * the "bottom" is +y.
 */
export class BoundingBox {
  x: number
  y: number
  w: number
  h: number

  constructor (x = 0, y = 0, width = 0, height = 0) {
    this.x = x
    this.y = y
    this.w = width
    this.h = height
  }

  clone () {
    return new BoundingBox(this.x, this.y, this.w, this.h)
  }

  /**
   * Push in (or pull out) all the sides of the box by a given amount. Returns null if too far. So squishing
   * { x: 0, y: 0, w: 2, h: 2} by 1/2 will give { x: 0.5, y: 0.5, w: 1, h: 1 }
   * @param margin {number}
   */
  squish (margin = 0) {
    const { x, y, w, h } = this

    if (2 * margin > w || 2 * margin > h) return null

    return new BoundingBox(
      x + margin,
      y + margin,
      w - 2 * margin,
      h - 2 * margin
    )
  }

  squishAsymmetrically (
    left = 0,
    right = 0,
    bottom = 0,
    top = 0,
    flipY = false
  ) {
    const { x, y, w, h } = this

    if (2 * (left + right) > w || 2 * (bottom + top) > h) {
      return null
    }

    if (flipY) {
      let tmp = bottom
      bottom = top
      top = tmp
    }

    return new BoundingBox(
      x + left,
      y + top,
      w - (left + right),
      h - (top + bottom)
    )
  }

  translate (v) {
    return new BoundingBox(this.x + v.x, this.y + v.y, this.w, this.h)
  }

  scale (s) {
    return new BoundingBox(this.x * s, this.y * s, this.w * s, this.h * s)
  }

  getX2 () {
    return this.x + this.w
  }

  getY2 () {
    return this.y + this.h
  }

  /**
   * Attempt to convert an object to a bounding box via various interpretation methods. In particular, the relevant
   * parameters are x (or x1), y (or y1), x2, y2, w (or width), h (or height), cx (or centerX), and cy (or centerY).
   * @param obj
   */
  static fromObj (obj: BoundingBoxLike): BoundingBox {
    let finalX1, finalY1, finalX2, finalY2

    if (Array.isArray(obj)) {
      finalX1 = obj[0]
      finalY1 = obj[1]
      finalX2 = obj[2] + finalX1
      finalY2 = obj[3] + finalY1
    } else if (typeof obj === 'object') {
      if (obj instanceof BoundingBox) {
        return obj.clone()
      }

      let { x, y, x1, y1, x2, y2, w, h, width, height, cx, cy, centerX, centerY } = obj

      // various aliases
      x = x ?? x1
      y = y ?? y1

      w = w ?? width
      h = h ?? height

      cx = cx ?? centerX
      cy = cy ?? centerY

      // We wish to find a rectangle that is roughly consistent. Note that along each axis, we have four relevant
      // variables: x, x2, w, cx. The axes are totally separable, so the problem is pretty trivial. I'm too tired
      // to figure out how to do it elegantly rather than case work.
      ;[finalX1, finalX2] = resolveAxisSpecification(x, x2, w, cx)
      ;[finalY1, finalY2] = resolveAxisSpecification(y, y2, h, cy)
    }

    finalX1 = +finalX1
    finalY1 = +finalY1
    finalX2 = +finalX2
    finalY2 = +finalY2

    if (finalX1 !== finalX1 || finalY1 !== finalY1 || finalX2 !== finalX2 || finalY2 !== finalY2) {
      throw new Error('BoundingBox.fromObj: Invalid bounding box specification')
    }

    return new BoundingBox(
      finalX1,
      finalY1,
      finalX2 - finalX1,
      finalY2 - finalY1
    )
  }

  get x1 () {
    return this.x
  }

  get y1 () {
    return this.y
  }

  get x2 () {
    return this.getX2()
  }

  get y2 () {
    return this.getY2()
  }

  tl () {
    return new Vec2(this.x, this.y)
  }

  static Transform = Object.freeze({
    X: (x: number | NumericArray, box1: BoundingBox, box2: BoundingBox, flipX: boolean) => {
      if (Array.isArray(x) || isTypedArray(x)) {
        x = x as NumericArray

        for (let i = 0; i < x.length; ++i) {
          let fractionAlong = (x[i] - box1.x) / box1.w
          if (flipX) fractionAlong = 1 - fractionAlong
          x[i] = fractionAlong * box2.w + box2.x
        }
        return x
      } else {
        x = x as number
        return BoundingBox.Transform.X([ x ], box1, box2, flipX)[0]
      }
    },
    Y: (y: number | NumericArray, box1: BoundingBox, box2: BoundingBox, flipY: boolean) => {
      if (Array.isArray(y) || isTypedArray(y)) {
        y = y as NumericArray
        for (let i = 0; i < y.length; ++i) {
          let fractionAlong = (y[i] - box1.y) / box1.h
          if (flipY) fractionAlong = 1 - fractionAlong
          y[i] = fractionAlong * box2.h + box2.y
        }
        return y
      } else {
        y = y as number
        return BoundingBox.Transform.Y([y], box1, box2, flipY)[0]
      }
    },
    XY: (xy: NumericArray, box1: BoundingBox, box2: BoundingBox, flipX: boolean, flipY: boolean) => {
      if (Array.isArray(xy) || isTypedArray(xy)) {
        xy = xy as NumericArray
        for (let i = 0; i < xy.length; i += 2) {
          let fractionAlong = (xy[i] - box1.x) / box1.w

          if (flipX) fractionAlong = 1 - fractionAlong

          xy[i] = fractionAlong * box2.w + box2.x

          fractionAlong = (xy[i + 1] - box1.y) / box1.h

          if (flipY) fractionAlong = 1 - fractionAlong

          xy[i + 1] = fractionAlong * box2.h + box2.y
        }
        return xy
      } else {
        throw new TypeError("BoundingBox.Transform.XY only accepts numeric arrays (Arrays of numbers and typed arrays)")
      }
    },
    getReducedTransform (box1: BoundingBox, box2: BoundingBox, flipX: boolean, flipY: boolean): Reduced2DBoundingBoxTransform {
      let xm = 1 / box1.w
      let xb = -box1.x / box1.w

      if (flipX) {
        xm *= -1
        xb = 1 - xb
      }

      xm *= box2.w
      xb *= box2.w
      xb += box2.x

      let ym = 1 / box1.h
      let yb = -box1.y / box1.h

      if (flipY) {
        ym *= -1
        yb = 1 - yb
      }

      ym *= box2.h
      yb *= box2.h
      yb += box2.y

      return { xm, xb, ym, yb }
    }
  })
}

// Simplified 2D transform. The formula to transform a point (x, y) is R^2 -> R^2: (xm * x + xb, ym * y + yb).
export type Reduced2DBoundingBoxTransform = { xm: number, xb: number, ym: number, yb: number }

const EMPTY = new BoundingBox(0, 0, 0, 0)

// TODO fix
function intersectBoundingBoxes (box1, box2) {
  let x1 = Math.max(box1.x, box2.x)
  let y1 = Math.max(box1.y, box2.y)
  let x2 = Math.min(box1.x2, box2.x2)
  let y2 = Math.min(box1.y2, box2.y2)

  if (x2 < x1) {
    return EMPTY.clone()
  }

  if (y2 < y1) {
    return EMPTY.clone()
  }

  let width = x2 - x1
  let height = y2 - y1

  return new BoundingBox(x1, y1, width, height)
}
