import { Color, ColorLike } from "./color.js";
import { CompositionType } from "./composition_type.js";
import { deepEquals, staticImplements } from "../utils.js";

/**
 * Partial pen specification, with optional members
 */
export type PenLike = ({
  [key in keyof Pen]?: Pen[key]
} & {
  color: ColorLike
}) | Pen

/**
 * Describes how a line should be drawn. Opacity should be set through color and is not tracked separately.
 */
export class Pen {
  color: Color
  thickness: number
  dashPattern: number[]
  dashOffset: number
  endcap: keyof typeof Pen.ENDCAP_TYPES
  endcapRes: number
  join: keyof typeof Pen.JOIN_TYPES
  joinRes: number
  useNative: boolean
  visible: boolean

  // Enum for endcaps
  static ENDCAP_TYPES = Object.freeze({
    butt: 0,
    round: 1,
    square: 2
  } as const)

  // Enum for join types
  static JOIN_TYPES = Object.freeze({
    bevel: 0,
    miter: 2,
    round: 1,
    dynamic: 3
  } as const)

  constructor () {

  }

  static compose (...args: PenLike[]): Pen {
    let p = Pen.default()

    // Later arguments are given more precedence
    for (let spec of args) {
      if (spec.color !== undefined)
        p.color = Color.fromObj(spec.color)
      if (spec.thickness !== undefined)
        p.thickness = spec.thickness
      if (spec.dashPattern !== undefined)
        p.dashPattern = spec.dashPattern
      if (spec.dashOffset !== undefined)
        p.dashOffset = spec.dashOffset
      if (spec.endcap !== undefined)
        p.endcap = spec.endcap
      if (spec.endcapRes !== undefined)
        p.endcapRes = spec.endcapRes
      if (spec.join !== undefined)
        p.join = spec.join
      if (spec.joinRes !== undefined)
        p.joinRes = spec.joinRes
      if (spec.useNative !== undefined)
        p.useNative = spec.useNative
      if (spec.visible !== undefined)
        p.visible = spec.visible
    }

    p.color = p.color.clone()

    return p
  }

  static create (params: PenLike): Pen {
    return Pen.compose(params)
  }

  static default (): Pen {
    let p = new Pen()

    p.color = new Color(0, 0, 0, 255)
    p.thickness = 2
    p.dashPattern = []
    p.dashOffset = 0
    p.endcap = 'round'
    p.endcapRes = 1
    p.join = 'dynamic'
    p.joinRes = 1
    p.useNative = false
    p.visible = true

    return p
  }

  static fromObj (o: unknown): Pen {
    if (typeof o === 'string') return _interpretStringAsPen(o)

    return Pen.compose(Pen.default(), o as PenLike)
  }

  _toJoinTypeEnum (): number {
    return Pen.JOIN_TYPES[this.join]
  }

  _toEndcapTypeEnum (): number {
    return Pen.ENDCAP_TYPES[this.endcap]
  }

  equals (p: Pen): boolean {
    return deepEquals(this, p)
  }
}

// TODO
function _interpretStringAsPen (str): Pen {
  try {
    let color = Color.fromCss(str)

    return Pen.fromObj({ color })
  } catch {
    return Pen.default()
  }
}

// Ensure Pen implements requisite composition type members
staticImplements<CompositionType<Pen, PenLike>>(Pen)
