import { Complex } from "./normal.js"
import { Color } from "../other/color.js";

function approxAtan (x: number): number { // positive x only
  if (x > 1) return (Math.PI / 2 - approxAtan(1 / x))
  return (Math.PI / 4) * x - x * (x - 1) * (0.2447 + 0.0663 * x);
}

const scratch = new Uint8ClampedArray(4)

type ColorSchemeArrayInput = Complex[] | Float64Array | Float32Array

// Method of converting complex to RGBA
abstract class ColoringScheme {
  abstract writeComplexArrayToRGBA (c: ColorSchemeArrayInput, arr: Uint8ClampedArray, index: number): void;

  writeComplexToRGBA (c: Complex, arr: Uint8ClampedArray, index: number) {
    this.writeComplexArrayToRGBA([c], arr, index)
  }

  /**
   * Convert a complex number via this scheme. Don't use this function directly, unless only invoking it occasionally;
   * writeComplexArrayToRGBA is usually preferable.
   * @param c
   */
  complexToColor(c: Complex): Color {
    this.writeComplexArrayToRGBA([ c ], scratch, 0)

    return new Color(scratch[0], scratch[1], scratch[2], scratch[3])
  }
}

function writeRGBA (r: number, g: number, b: number, a: number, arr: Uint8ClampedArray, index: number) {
  arr[index] = (r * 255) | 0
  arr[index + 1] = (g * 255) | 0
  arr[index + 2] = (b * 255) | 0
  arr[index + 3] = (a * 255) | 0
}

function writeHL (h: number, l: number, arr: Uint8ClampedArray, index: number) {
  let q = l < 0.5 ? l * 2 : 1;
  let p = 2 * l - q;

  h -= 1 / 3
  if (h < 0) h += 1
  if (h > 1) h -= 1

  let r = 0, g = 0, b = 0, d = (q - p) * 6

  // h is between 0 and 1
  if (h < 1 / 6) {
    b = p + d * h
    g = q
    r = p
  } else if (h < 1 / 3) {
    b = q
    g = p + d * (1 / 3 - h)
    r = p
  } else if (h < 1 / 2) {
    b = q
    g = p
    r = p + d * (h - 1 / 3)
  } else if (h < 2 / 3) {
    b = p + d * (2 / 3 - h)
    g = p
    r = q
  } else if (h < 5 / 6) {
    b = p
    g = p + d * (h - 2 / 3)
    r = q
  } else {
    b = p
    g = q
    r = p + d * (1 - h)
  }

  arr[index] = (r * 255) | 0
  arr[index + 1] = (g * 255) | 0
  arr[index + 2] = (b * 255) | 0
  arr[index + 3] = 255
}

function isComplexArray (c: ColorSchemeArrayInput): c is Complex[] {
  return c[0] instanceof Complex
}

type ColorSchemeOptions = {
  // default is "normal"
  type?: "normal" | "repeat" | "repeat_exponential"
  base?: number
  repeating?: boolean
  transformation?: "atan"
  minLightness?: number
  maxLightness?: number
}

// A repeating color scheme resets its brightness at various magnitudes. "exponential" will reset at every integer power
// of the base. "even" will reset at every multiple of the base.
export class StandardColoringScheme extends ColoringScheme {
  type: "normal" | "repeat" | "repeat_exponential"
  minLightness: number
  maxLightness: number
  base: number
  transformation: "atan"  // ...

  constructor (opts: ColorSchemeOptions={}) {
    super()

    this.type = opts.type ?? "normal"

    let base = +(opts.base ?? 2)
    if (base !== base || base < 0) {
      throw new Error("Invalid color scheme repeating base")
    }

    this.base = opts.base ?? 2
    this.transformation = opts.transformation ?? "atan"
    this.minLightness = opts.minLightness ?? 0.2
    this.maxLightness = opts.maxLightness ?? 0.6

    if (this.minLightness < 0 || this.minLightness > 1) {
      throw new Error("Invalid color scheme minLightness")
    }

    if (this.maxLightness < 0 || this.maxLightness > 1) {
      throw new Error("Invalid color scheme maxLightness")
    }
  }

  writeComplexArrayToRGBA(complexArr: ColorSchemeArrayInput, arr: Uint8ClampedArray, index: number) {
    // Somewhat optimized

    if (complexArr.length === 0) return

    // Destructuring remains slow in Chrome, unfortunately, so do this
    let type = this.type, base = this.base, transformation = this.transformation
    let minLightness = this.minLightness
    let maxLightness = this.maxLightness
    let rLightness = maxLightness - minLightness

    let isNormal = type === "normal"
    let isExponential = type === "repeat_exponential"
    let logBase = Math.log(this.base)

    function write (re: number, im: number, index: number) {
      let arg = Complex.approxArgComponents(re, im)
      let magnitude = Math.sqrt(re * re + im * im)

      let h = (arg + 2 * Math.PI / 3) * (1 / (2 * Math.PI))

      if (isNormal) {
        writeHL(h, 2 / Math.PI * approxAtan(magnitude), arr, index)
        return
      } else if (isExponential) {
        // Take the logarithm with the base TODO fast logarithm lookup, etc
        magnitude = Math.log(magnitude) / logBase
      } else {
        magnitude /= base
      }

      // Normalize to [minLightness, maxLightness), repeating
      magnitude = minLightness + rLightness * (magnitude - Math.floor(magnitude))

      writeHL(h, 4 / Math.PI * approxAtan(magnitude), arr, index)
    }

    if (isComplexArray(complexArr)) {
      for (let i = 0; i < complexArr.length; ++i) {
        let c = complexArr[i]
        let re = c.re, im = c.im

        write(re, im, index + (i << 2))
      }
    } else if (complexArr instanceof Float32Array) {
      // Split like this so that the stride is separate for typed arrays and hopefully won't cause deoptimization... lol
      for (let i = 0; i < complexArr.length; i += 2) {
        let re = complexArr[i], im = complexArr[i + 1]

        write(re, im, index + (i << 1))
      }
    } else {
      for (let i = 0; i < complexArr.length; i += 2) {
        let re = complexArr[i], im = complexArr[i + 1]

        write(re, im, index + (i << 1))
      }
    }
  }
}
