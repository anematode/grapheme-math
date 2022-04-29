import { Eventful } from './eventful.js'
import { getStringID, getVersionID } from '../utils.js'
import { Props } from './props.js'
import { Scene } from "./scene.js";
import { RenderingInfo } from "./renderer_instruction.js";

export type ElementOptions = {
  id?: string
}

export type ElementInternalStore = {
  version: number,
  renderInfo?: RenderingInfo | null
}

/**
 * The element class.
 */
export class Element extends Eventful {
  // Unique string id of this element
  id: string

  // Parent of this element
  parent: Element|null

  // Parent scene (top-level element)
  scene: Scene|null

  /**
   * Properties of this element. Stores most of the state of the element. Similar to internal but with a lot more
   * predefined behavior
   */
  props: Props

  /**
   * -1 corresponds to an element that has just been created, added, or removed. 0 corresponds to an element which
   * needs an update. 100 corresponds to a completely updated element
   */
  updateStage: number

  /**
   * Used for storing intermediate results required for rendering, interactivity and other things, to avoid cluttering
   * the main element properties, and to indicate they should generally not be touched by an external program
   */
  internal: ElementInternalStore

  constructor (opts: ElementOptions = {}) {
    super() // Eventful

    this.id = opts.id ?? getStringID()

    if (typeof this.id !== 'string' || this.id.length === 0)
      throw new TypeError('The element id must be a non-empty string')

    this.parent = null
    this.scene = null
    this.props = new Props()
    this.updateStage = -1
    this.internal = {
      version: getVersionID()
    }

    // Call the element-defined constructor
    this.init(opts)
  }

  /**
   * Derived class–implemented function that is called when the element needs to be updated (usually to change how
   * it's displayed)
   */
  _update () {}

  /**
   * Add a given element as a child to this element. Fails on elements that are not groups.
   * @param e Element to add
   */
  add (e: Element) {
    throw new Error("Element is not a group and does not support having children")
  }

  /**
   * Apply a given function, accepting a single argument (the element)
   * @param callback The callback function
   */
  apply (callback: (e: Element) => void) {
    callback(this)
  }

  /**
   * Inherit properties from the parent. If the updateStage is -1, then it indicates the child has not inherited any
   * properties yet at all, so we need to check them all.
   */
  _defaultInheritProps () {
    if (this.parent)
      this.props.inheritPropertiesFrom(
        this.parent.props,
        this.updateStage === -1
      )
  }

  /**
   * Default rendering info information, which just pulls from internal.renderInfo
   */
  getRenderingInfo () {
    if (this.internal.renderInfo)
      return this.internal.renderInfo
  }

  /**
   * Check whether a given element is a child of this element
   * @param child
   * @param recursive Whether to search recursively, or just look for immediate children
   */
  isChild (child: Element, recursive = true) {
    return false
  }

  /**
   * Whether this element is a top-level scene
   */
  isScene () {
    return false
  }

  init (params) {}

  _defaultComputeProps () {

  }

  /**
   * Recursively set the scene which this element belongs to
   * @param scene The scene
   */
  _setScene (scene) {
    this.scene = scene
  }

  /**
   * Stringify the props contents of this element
   */
  stringify () {
    this.props.stringify()
  }

  update () {
    // If some properties have changed, set the update stage accordingly. We use .min in case the update stage is -1
    if (this.props.hasChangedProperties)
      this.updateStage = Math.min(this.updateStage, 0)

    if (this.updateStage === 100) return

    this._update()

    this.updateStage = 100
  }

  getChildren (): Element[] {
    // @ts-ignore
    return this.isGroup() ? this.children : []
  }

  isGroup (): boolean {
    return false
  }
}
