
class Complex {
  constructor (re=0, im=0) {
    this.re = +re
    this.im = +im
  }

  /**
   * Attempt to parse an object (returns an undefined Complex if couldn't sensibly convert)
   * @param o {any}
   * @return {Complex}
   */
  static fromObj (o) {
    let re = NaN, im = NaN
    if (typeof o === "number") {
      re = o
      im = 0
    } else if (typeof o === "object") {
      if (Array.isArray(o)) {
       re = +o[0]
       im = +g[1]
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
}

export { Complex }
