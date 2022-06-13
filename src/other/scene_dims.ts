import { BoundingBox } from "./bounding_box.js"

/**
 * Passed to children as the parameter "sceneDimensions"
 */
export class SceneDimensions {
  width: number
  height: number
  dpr: number

  canvasWidth: number
  canvasHeight: number

  constructor (width: number, height: number, dpr: number) {
    this.width = width
    this.height = height
    this.dpr = dpr

    // The size of the canvas in true device pixels, rather than CSS pixels
    this.canvasWidth = this.dpr * this.width
    this.canvasHeight = this.dpr * this.height
  }

  /**
   * Get the bounding box of the entire scene
   * @returns
   */
  getBoundingBox (): BoundingBox {
    return new BoundingBox(0, 0, this.width, this.height)
  }

  clone(): SceneDimensions {
    return new SceneDimensions(this.width, this.height, this.dpr)
  }
}
