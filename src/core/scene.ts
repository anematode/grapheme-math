import { Group } from './group.js'
import { BoundingBox } from '../other/bounding_box.js'
import { SceneDimensions } from "../other/scene_dims.js"

const DEFAULT_SCENE_DIMS = new SceneDimensions(640, 480, 1)
const MIN_SIZE = 100, MAX_SIZE = 16384
const MIN_DPR = 0.1, MAX_DPR = 16

function checkDimInRange(d: number) {
  if (typeof d !== "number" || Number.isNaN(d) || d < MIN_SIZE || d > MAX_SIZE) {
    throw new RangeError(`Dimension ${d} is out of range [${MIN_SIZE}, ${MAX_SIZE}]`)
  }
}

function checkDPRInRange(dpr: number) {
  if (typeof dpr !== "number" || Number.isNaN(dpr) || dpr < MIN_DPR || dpr > MAX_DPR) {
    throw new RangeError(`Device pixel ratio ${dpr} is out of range [${MIN_DPR}, ${MAX_DPR}]`)
  }
}

/**
 * Top level element in a Grapheme context. The scene has a width, height, and device pixel ratio as its defining
 * geometric patterns, and potentially other properties -- interactivity information, for example. Uniquely, every
 * element knows its scene directly as its .scene property.
 */
export class Scene extends Group {
  init (params) {
    this.scene = this

    this.props.set('sceneDims', new SceneDimensions(640, 480, 1))
    this.props.setPropertyInheritance('sceneDims', true)
  }

  /**
   * Get the scene's dimensions
   */
  getDims (): SceneDimensions {
    return this._getDims().clone()
  }

  _getDims (): SceneDimensions {
    return this.props.get('sceneDims')
  }

  _setDims (dims: SceneDimensions) {
    this.props.set("sceneDims", dims, 0, 2)
  }

  setWidth (w: number): Scene {
    checkDimInRange(w)
    let d = this.getDims()
    d.width = w

    this._setDims(d)
    return this
  }

  setHeight (h: number): Scene {
    checkDimInRange(h)
    let d = this.getDims()
    d.width = h

    this._setDims(d)
    return this
  }

  setDPR (dpr: number): Scene {
    checkDimInRange(dpr)
    let d = this.getDims()
    d.width = dpr

    this._setDims(d)
    return this
  }

  /**
   * Compute the internal property "sceneDimensions"
   */
  calculateSceneDimensions () {
    const { props } = this

    if (props.haveChanged(['width', 'height', 'dpr'])) {
      const { width, height, dpr } = props.proxy
      const sceneDimensions = new SceneDimensions(width, height, dpr)

      // Equality check of 2 for deep comparison, in case width, height, dpr have not actually changed
      props.set(
        'sceneDims',
        sceneDimensions,
        0 /* real */,
        2 /* equality check */
      )
    }
  }

  updateProps () {
    this._defaultComputeProps()
    this.calculateSceneDimensions()
  }

  /**
   * Only scenes (and derived scenes) return true
   * @returns {boolean}
   */
  isScene () {
    return true
  }

  _update () {
    this.updateProps()

    this.internal.renderInfo = {
      contexts: [{
        insnType: 'scene',
        dims: this.props.get('sceneDims'),
        backgroundColor: this.props.get('backgroundColor')
      }]
    }
  }

  /**
   * This function updates all the elements and is the only one with the authority to mark all properties, including
   * inheritable properties, as unchanged.
   */
  updateAll () {
    this.apply(child => {
      child.update()
    })

    // Mark the update as completed (WIP)
    this.apply(child => child.props._markGlobalUpdateComplete())
  }
}
