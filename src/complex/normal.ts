import {gammaReal} from "../real/normal.js"
import {lanczosCoefficients} from "../real/gamma.js"

/**
 * Normal-precision complex number.
 */
class Complex {
  re: number;
  im: number;

  constructor (re=0, im=0) {
    this.re = +re
    this.im = +im
  }

  /**
   * Attempt to parse an object. Returns a Complex with NaN parts if couldn't sensibly convert.
   * @param o {any}
   * @return {Complex}
   */
  static fromObj (o: any): Complex {
    let re = NaN, im = NaN
    if (typeof o === "number") {
      re = o
      im = 0
    } else if (typeof o === "object") {
      if (Array.isArray(o)) {
       re = +o[0]
       im = +o[1]
      } else if (o.x !== undefined && o.y !== undefined) {
        re = +o.x
        im = +o.y
      } else if (o.re !== undefined && o.im !== undefined) {
        re = +o.re
        im = +o.im
      }
    }

    return new Complex(re, im)
  }

  /**
   * Add two complex numbers and write the result to this complex number.
   * @param c1
   * @param c2
   */
  add (c1: Complex, c2: Complex) {
    let c1r = c1.re, c2r = c2.re, c1i = c1.im, c2i = c2.im
    this.re = c1r + c2r
    this.im = c1i + c2i
  }

  multiply (c1: Complex, c2: Complex) {
    let c1r = c1.re, c2r = c2.re, c1i = c1.im, c2i = c2.im
    this.re = c1r * c2r - c1i * c2i
    this.im = c1r * c2i + c1i * c2r
  }

  multiplyReal (c: Complex, r: number) {
    this.re = c.re * r
    this.im = c.im * r
  }

  divide (c1: Complex, c2: Complex) {
    let c1r = c1.re, c2r = c2.re, c1i = c1.im, c2i = c2.im
    let d = 1 / (c2r * c2r + c2i * c2i)

    this.re = (c1r * c2r + c1i * c2i) * d
    this.im = (c1i * c2r - c1r * c2i) * d
  }

  reciprocal (c: Complex) {
    let cr = c.re, ci = c.im
    let denom = 1 / (cr * cr + ci * ci)

    this.re = cr * denom
    this.im = -ci * denom
  }

  subtract (c1: Complex, c2: Complex) {
    let c1r = c1.re, c2r = c2.re, c1i = c1.im, c2i = c2.im
    this.re = c1r - c2r
    this.im = c1i - c2i
  }

  sin (c: Complex) {
    let cr = c.re, ci = c.im

    this.re = Math.sin(cr) * Math.cosh(ci)
    this.im = Math.cos(cr) * Math.sinh(ci)
  }

  cos (c: Complex) {
    let cr = c.re, ci = c.im

    this.re = Math.cos(cr) * Math.cosh(ci)
    this.im = -Math.sin(cr) * Math.sinh(ci)
  }

  tan (c) {
    let cr = c.re, ci = c.im

    let denom = 1 / (Math.cos(2 * cr) + Math.cosh(2 * ci))

    this.re = Math.sin(2 * cr) * denom
    this.im = Math.sinh(2 * ci) * denom
  }

  exp (c) {
    let cr = c.re, ci = c.im
    let exp = Math.exp(cr)

    this.re = exp * Math.cos(ci)
    this.im = exp * Math.sin(ci)
  }

  log (c) {
    let cr = c.re, ci = c.im

    this.re = Math.log(Math.hypot(c.re, ci))
    this.im = Math.atan2(c.im, cr)
  }

  pow (c1: Complex, c2: Complex) {
    let c2i = c2.im

    if (c2i === 0) {
      this.powReal(c1, c2.re)
      return
    }

    let c = new Complex()

    c.log(c1)
    c.multiply(c2, c)

    this.exp(c)
  }

  /**
   * Set this complex number to the result of b^r
   * @param b {Complex}
   * @param r {number}
   */
  powReal (b: Complex, r: number) {
    if (Number.isInteger(r)) {
      if (r === 0) { // TODO add more integer handling
        this.re = 1
        this.im = 0
        return
      }
    }

    let c = new Complex()

    c.log(b)
    c.multiplyReal(c, r)

    this.exp(c)
  }

  static abs (c: Complex): number {
    return Math.hypot(c.re, c.im) // kinda slow, but whatevs
  }

  static absSquared (c: Complex): number {
    return c.re * c.re + c.im * c.im
  }

  static arg (c: Complex): number {
    return Math.atan2(c.im, c.re)
  }

  /**
   * Get the approximate argument of a complex number, quickly (for applications like domain coloring, where accuracy
   * is not essential)
   * @param c {Complex}
   * @returns {number}
   */
  static approxArg (c: Complex): number {
    // Credit to https://math.stackexchange.com/a/1105038/677124
    let x = c.im, y = c.re

    if (!Number.isFinite(x) || !Number.isFinite(y)) {
      return Math.atan2(y, x) // will probably be fast bc of special handlers within atan2
    }

    let a = Math.min(x, y) / Math.max(x, y)
    let s = a * a
    let r = ((-0.0464964749 * s + 0.15931422) * s - 0.327622764) * s * a + a

    if (Math.abs(y) > Math.abs(x)) r = Math.PI / 2 - r
    if (x < 0) r = Math.PI - r
    if (y < 0) r = -r
    return r
  }

  gamma (z: Complex) {
    let zi = z.im, zr = z.re
    if (zi < 1e-15) {
      this.re = gammaReal(zr)
      this.im = 0

      //return
    }

    if (!Number.isFinite(zi) || !Number.isFinite(zr)) {
      if (zi !== zi || zr !== zr) {
        this.re = this.im = NaN
        return
      }

      // TODO handle cases
      this.re = this.im = NaN
      return
    }

    if (zr < 0.5) {
      // Reflection formula

      let c = new Complex(1 - this.re, -this.im)
      let sz = new Complex()
      sz.multiplyReal(z, Math.PI)
      sz.sin(sz)

      c.multiply(c, sz)

      this.divide(new Complex(Math.PI, 0), c)
    }

    zr -= 1
    let sr = 0.99999999999980993, si = 0
    let dRe = 0, dIm = zi

    for (let i = 0; i < 8; ++i) {
      dRe = zr + i + 1

      // Compute coeff[i] / (dRe + i * dIm)

      let denom = lanczosCoefficients[i] / (dRe * dRe + dIm * dIm)

      sr += dRe * denom
      si += -dIm * denom
    }

    let t = new Complex(zr + 7.5, zi)
    let sqrt2Pi = new Complex(2.5066282746310007, 0)
    let tPow = new Complex()
    tPow.pow(t, new Complex(zr + 0.5, zi))
    let tExp = new Complex()
    tExp.exp(new Complex(-t.re, -t.im))

    t.multiply(sqrt2Pi, tPow)
    t.multiply(new Complex(sr, si), t)
    this.multiply(tExp, t)
  }
}

export { Complex }
