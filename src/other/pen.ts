import { Color } from "./color";

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
}
