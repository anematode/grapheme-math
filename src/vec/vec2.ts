// Another one of these, yada yada, reinventing the wheel, yay
export class Vec2 {
  x: number
  y: number

  constructor (x: number, y: number) {
    this.x = x
    this.y = y
  }

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
