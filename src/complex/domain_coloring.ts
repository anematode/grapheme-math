import {Complex} from "./normal.js"

type RGB = [ number, number, number ]

function hue2rgb(p, q, t) {
    if (t < 0) t += 1
    else if (t > 1) t -= 1

    if (t < 1 / 6) return p + (q - p) * 6 * t;
    if (t < 1 / 2) return q;
    if (t < 2 / 3) return p + (q - p) * (2 / 3 - t) * 6;

    return p;
}

function hslToRGB(h: number, s: number, l: number): RGB {
    let r, g, b;

    if (s === 0) {
        r = g = b = l; // achromatic
    } else {
        let q = l < 0.5 ? l * (1 + s) : l + s - l * s;
        let p = 2 * l - q;
        r = hue2rgb(p, q, h + 1 / 3);
        g = hue2rgb(p, q, h);
        b = hue2rgb(p, q, h - 1 / 3);
    }

    return [Math.round(r * 255), Math.round(g * 255), Math.round(b * 255)];
}

export function complexToRGB (c: Complex): RGB {
  let h = (Complex.arg(c) + 2 * Math.PI / 3) / (2 * Math.PI)
  let s = 1
  let l = 2 / Math.PI * Math.atan(Complex.abs(c))

  return hslToRGB(h, s, l)
}

function fastAtan (x: number): number { // positive x only
    if (x > 1) return (Math.PI / 2 - fastAtan(1 / x))
    return (Math.PI / 4) * x - x * (x - 1) * (0.2447 + 0.0663 * x);
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
    if (h < 1/6) {
        b = p + d * h
        g = q
        r = p
    } else if (h < 1/3) {
        b = q
        g = p + d * (1 / 3 - h)
        r = p
    } else if (h < 1/2) {
        b = q
        g = p
        r = p + d * (h - 1/3)
    } else if (h < 2/3) {
        b = p + d * (2 / 3 - h)
        g = p
        r = q
    } else if (h < 5/6) {
        b = p
        g = p + d * (h - 2/3)
        r = q
    } else {
        b = p
        g = q
        r = p + d * (1 - h)
    }

    arr[index] = (r * 255) | 0
    arr[index+1] = (g * 255) | 0
    arr[index+2] = (b * 255) | 0
    arr[index+3] = 255
}
