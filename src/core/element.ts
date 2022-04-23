import { Eventful } from './eventful.js'
import { getStringID, getVersionID } from '../grapheme_shared.js'
import { Props } from './props.js'
import { NullInterface } from './interface.js'
import { Scene } from "./scene";

type ElementOptions = {
  id?: string
}

export type ElementInternalStore = {
  version: number,
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
      throw new TypeError('The element id must be a non-empty string.')

    this.parent = null
    this.scene = null
    this.props = new Props()
    this.updateStage = -1
    this.internal = {
      version: getVersionID()
    }

    // Call the element-defined constructor
    this.init(opts)

    // Call set on remaining parameters. Corollary: constructor-only parameters should not also be parameters (no "id")
    this.set(opts)
  }

  /**
   * Inherited function that is called when the element needs to be updated (usually to change how it's displayed)
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
  apply (callback) {
    callback(this)
  }

  /**
   * Inherit properties from the parent. If the updateStage is -1, then it indicates the child has not inherited any
   * properties yet at all, so we need to check them all.
   */
  defaultInheritProps () {
    if (this.parent)
      this.props.inheritPropertiesFrom(
        this.parent.props,
        this.updateStage === -1
      )
  }

  getRenderingInfo () {
    if (this.internal.renderInfo) return this.internal.renderInfo
  }

  isChild (child, recursive = true) {
    return false
  }

  isScene () {
    return false
  }

  init (params) {}

  set (propName, value) {
    this.getInterface().set(this, propName, value)
  }

  get (propName) {
    return this.getInterface().get(this, propName)
  }

  getDict (propNames) {
    return this.getInterface().getDict(this, propNames)
  }

  /**
   * For all given properties, check which ones need to be filled in with default values.
   */
  defaultComputeProps () {
    let inter = this.getInterface()
    const needsInitialize = this.updateStage === -1

    inter.computeProps(this.props, needsInitialize)
  }

  getInterface () {
    return NullInterface
  }

  setScene (scene) {
    this.scene = scene
  }

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
}
