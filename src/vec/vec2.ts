import { isTypedArray } from "../utils";

type CardinalDirection = "N" | "NE" | "E" | "SE" | "S" | "SW" | "W" | "NW" | "C"
export type Vec2Like = { x: number, y: number } | [ number, number ] | Vec2 | { im: number, re: number } | CardinalDirection
export type Vec2ArrayLike = Float32Array | Float64Array | Vec2[] | number[]

/**
 * Generic Vec2 class, reinventing the wheel...
 */
export class Vec2 {
  x: number
  y: number

  constructor (x: number, y: number) {
    this.x = x
    this.y = y
  }

  /**
   * Permissively convert to Vec2 from an arbitrary object, ideally something that is Vec2Like
   * @param obj
   */
  static fromObj (obj: unknown) {
    let x: number = NaN,
      y: number = NaN

    if (Array.isArray(obj)) {
      x = obj[0]
      y = obj[1]
    } else if (typeof obj === 'object' && (obj !== null)) {
      let o = obj as any
      if ('x' in obj) {
        x = +o.x
        y = +o.y
      } else if ('re' in obj) {
        x = +o.re
        y = +o.im
      }
    } else if (typeof obj === 'string') {
      switch (obj) {
        case 'N':
        case 'NE':
        case 'NW':
          y = 1
          break
        case 'S':
        case 'SE':
        case 'SW':
          y = -1
          break
        case 'C':
          y = 0
      }

      switch (obj) {
        case 'E':
        case 'NE':
        case 'SE':
          x = 1
          break
        case 'W':
        case 'NW':
        case 'SW':
          x = -1
          break
        case 'C':
          x = 0
      }
    }

    return new Vec2(x, y)
  }

  add (vec: Vec2) {
    return new Vec2(this.x + vec.x, this.y + vec.y)
  }

  subtract (vec: Vec2) {
    return new Vec2(this.x - vec.x, this.y - vec.y)
  }

  multiplyScalar (scalar) {
    return new Vec2(this.x * scalar, this.y * scalar)
  }

  rot (angle, centre) {
    // TODO
    let s = Math.sin(angle),
      c = Math.cos(angle)

    if (!centre)
      return new Vec2(c * this.x - s * this.y, s * this.x + c * this.y)
  }

  rotDeg (angle, centre) {
    return this.rot((angle * Math.PI) / 180, centre)
  }

  unit () {
    return this.multiplyScalar(1 / this.length())
  }

  length () {
    return Math.hypot(this.x, this.y)
  }

  lengthSquared () {
    return this.x * this.x + this.y * this.y
  }
}

function throwInconvertibleElement(elem: any, i: number): never {
  throw new TypeError(`Expected input to be convertible to a flat array of Vec2s, found element ${elem} at index ${i}`)
}

function throwOddLengthArray(len: number): never {
  throw new Error(`Expected input to be convertible to a flat array of Vec2s, got array of odd length ${len}`)
}

function vec2NonFlatArrayConversion (arr, f32 = true, throwOnError=false) {
  let ret = new (f32 ? Float32Array : Float64Array)(arr.length * 2)
  let retIndex = -1

  for (let i = 0; i < arr.length; ++i) {
    let elem = arr[i]

    if (elem.x) {
      ret[++retIndex] = elem.x
      ret[++retIndex] = elem.y
    } else if (Array.isArray(elem)) {
      if (elem.length !== 2) {
        if (throwOnError)
          throwInconvertibleElement(elem, i)

        return
      }

      ret[++retIndex] = elem[0]
      ret[++retIndex] = elem[1]
    } else {
      if (throwOnError)
        throwInconvertibleElement(elem, i)

      return
    }
  }

  return ret
}

/**
 * Flatten a given array relatively permissively into an flat array; returns undefined if the conversion failed
 * @param obj
 * @param f32
 * @param throwOnError
 */
export function vec2ArrayConversion (obj, f32 = true, throwOnError=false): Float32Array | Float64Array | undefined {
  if (Array.isArray(obj)) {
    for (let i = 0; i < obj.length; ++i) {
      if (typeof obj[i] !== 'number') {
        return vec2NonFlatArrayConversion(obj)
      }
    }

    // Obj is just an array of numbers
    if (obj.length % 2 === 1) {
      if (throwOnError)
        throwOddLengthArray(obj.length)

      return
    }

    return new (f32 ? Float32Array : Float64Array)(obj)
  } else if (isTypedArray(obj)) {
    if (obj.length % 2 === 1) {
      if (throwOnError)
        throwOddLengthArray(obj.length)

      return
    }


    if (f32 && obj instanceof Float32Array) return obj
    if (!f32 && obj instanceof Float64Array) return obj

    return new (f32 ? Float32Array : Float64Array)(obj)
  }

  if (throwOnError)
    throw new TypeError(`Could not convert object into flat Vec2 array`)
}
