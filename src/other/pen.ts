import { Color } from "./color";
import { CompositionType } from "./composition_type.js";
import { staticImplements } from "../utils.js";

export type PartialPenSpecification = {
  [key in keyof Pen]?: Pen[key],
  color: ColorSpecification
}

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

  static ENDCAP_TYPES = Object.freeze({
    butt: 0,
    round: 1,
    square: 2
  } as const)

  static JOIN_TYPES = Object.freeze({
    bevel: 0,
    miter: 2,
    round: 1,
    dynamic: 3
  } as const)

  constructor () {
    this.color = new Color(0, 0, 0, 255)
    this.thickness = 2
    this.dashPattern = []
    this.dashOffset = 0
    this.endcap = 'round'
    this.endcapRes = 1
    this.join = 'dynamic'
    this.joinRes = 1
    this.useNative = false
    this.visible = true
  }

  static compose (...args: PartialPenSpecification[]): Pen {
    let p = new Pen()

    // Later arguments are given more precedence
    for (let spec of args) {
      Object.assign(p, spec)
    }

    return p
  }

  static create (params: PartialPenSpecification): Pen {
    return new Pen()
  }

  static default () {
    return new Pen()
  }

  static fromObj (o: any): Pen {
    return new Pen()
  }

  _toJoinTypeEnum (): number {
    return Pen.JOIN_TYPES[this.join]
  }

  _toEndcapTypeEnum (): number {
    return Pen.ENDCAP_TYPES[this.endcap]
  }
}

// Ensure Pen implements requisite composition type members
staticImplements<CompositionType<Pen, PartialPenSpecification>>(Pen)
