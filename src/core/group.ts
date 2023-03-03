import { Element, ElementOptions } from './element.js'
import { Scene } from "./scene.js";

export class Group extends Element {
  children: Element[]

  constructor (params: ElementOptions) {
    super(params)

    this.children = []
  }

  _update () {
    this._defaultInheritProps()
  }

  /**
   * Add an element to this group.
   * @param elem Element to add
   * @returns This group, for chaining
   */
  add (elem) {
    if (elem.isScene()) throw new Error('Scene cannot be a child')
    if (elem.parent) throw new Error('Element to be added already has a parent')
    if (!(elem instanceof Element)) throw new TypeError('Element not element')
    if (elem === this) throw new Error("Can't add self")
    if (elem.isChild(this)) throw new Error("Can't make cycle")

    this.children.push(elem)
    elem.parent = this
    elem._setScene(this.scene)

    elem.updateStage = -1
    this._markNeedsOrdering()

    return this
  }

  /**
   * Run callback(element) on this element and all the element's children
   * @param callback {Function}
   */
  apply (callback: (childElem: Element) => void) {
    callback(this)

    this.children.forEach(child => child.apply(callback))
  }

  /**
   * If some inheritable properties have changed since the last global update completion, set all the children's update
   * stages to 0. May change how this works later
   */
  informChildrenOfInheritance () {
    if (this.props.lastInheritableChanged /* TODO */ && this.children) {
      this.children.forEach(child => {
        let st = child.updateStage
        child.updateStage = (st < 0) ? 0 : st
      })
    }
  }

  isChild (elem, recursive = true): boolean {
    for (const child of this.children) {
      if (child === elem) return true
      if (recursive && child.isChild(elem, true)) return true
    }

    return false
  }

  isGroup (): boolean {
    return true
  }

  _markNeedsOrdering () {
    this.internal.needsOrdering = true
  }

  sortChildren (force: boolean = false) {
    if (force || this.internal.needsOrdering) {
      let children = this.children

      if (children.length <= 1) return

      let cl = children.length
      let orders: number[] = []
      let indices: number[] = []

      for (let i = 0; i < cl; ++i) {
        orders.push(children[i].getOrder())
        indices.push(i)
      }

      // Doing it this way reduces the number of accesses to children
      indices.sort((i1, i2) => {
        let o1 = orders[i1], o2 = orders[i2]
        return (o1 < o2) ? -1 : ((o1 > o2) ? 1 : 0)
      })

      // Might be a little slow... TODO perf testing
      let oldC = children.slice()

      // Swap according to new indices, in place
      for (let i = 0; i < cl; ++i) {
        children[i] = oldC[indices[i]]
      }
    }
  }

  /**
   * Remove a direct element from this group. Fails silently if the passed element is not a child.
   * @param elem Element to remove
   * @returns This group, for chaining
   */
  remove (elem: Element): Group {
    const index = this.children.indexOf(elem)

    if (index !== -1) {
      this.children.splice(index, 1)
      elem.parent = null
      elem._setScene(null)

      elem.updateStage = -1
    }

    this._markNeedsOrdering()

    return this
  }

  _setScene (scene: Scene) {
    this.scene = scene

    for (let i = 0; i < this.children.length; i++) {
      this.children[i]._setScene(scene)
    }
  }

  triggerEvent (eventName, data: any): boolean {
    // Children are triggered first
    for (const child of this.children) {
      if (child.triggerEvent(eventName, data)) return true
    }

    return super.triggerEvent(eventName, data)
  }

  update () {
    super.update()
    this.informChildrenOfInheritance()
  }
}
