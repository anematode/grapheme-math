import { Color } from "./color";
import { CompositionType } from "./composition_type.js";
import { staticImplements } from "../utils.js";

export type PartialPenSpecification = {
  [key in keyof Pen]?: Pen[key]
}

export class Pen {
  color: Color
  thickness: number
  dashPattern: number[]
  dashOffset: number
  endcap: 'round'
  endcapRes: number
  join: 'dynamic'
  joinRes: number
  useNative: boolean
  visible: boolean

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
    return new Pen()
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
}

// Ensure Pen implements requisite composition type members
staticImplements<CompositionType<Pen, PartialPenSpecification>>(Pen)
