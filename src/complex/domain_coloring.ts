import { Complex } from "./normal.js"
import { Color } from "../other/color.js";

function fastAtan (x: number): number { // positive x only
  if (x > 1) return (Math.PI / 2 - fastAtan(1 / x))
  return (Math.PI / 4) * x - x * (x - 1) * (0.2447 + 0.0663 * x);
}

// Method of converting complex to RGBA
abstract class ColoringScheme {
  writeComplexArrayToRGBA: (c: Complex[], arr: Uint8ClampedArray, index: number, colorScale: number) => void;

  writeComplexToRGBA (c: Complex, arr: Uint8ClampedArray, index: number, colorScale: number) {
    this.writeComplexArrayToRGBA([c], arr, index, colorScale)
  }

  complexToColor: (c: Complex) => Color;
}

export function writeComplexToRGBA (c: Complex, arr: Uint8ClampedArray, index: number, colorScale: number) {
  // Somewhat optimized
  let re = c.re, im = c.im

  const TWO_PI_OVER_3 = 2 * Math.PI / 3
  const TWO_OVER_PI = 2 / Math.PI

  let arg = 0 // argument

  if (!Number.isFinite(re) || !Number.isFinite(im)) {
    arg = Math.atan2(im, re)
  } else {
    let absX = Math.abs(re), absY = Math.abs(im)

    let a = (absX > absY) ? (absY / absX) : (absX / absY)
    let s = a * a
    arg = ((-0.0464964749 * s + 0.15931422) * s - 0.327622764) * s * a + a

    if (absY > absX) arg = Math.PI / 2 - arg
    if (re < 0) arg = Math.PI - arg
    if (im < 0) arg = -arg
  }

  let h = (arg + TWO_PI_OVER_3) * (1 / (2 * Math.PI))
  let l = TWO_OVER_PI * fastAtan(Math.sqrt(re * re + im * im) / colorScale)

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
