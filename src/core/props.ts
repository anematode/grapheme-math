import { deepEquals, getVersionID } from '../utils.js'

/**
 * The properties class stores an element's internal properties, in contrast to the user-facing properties. There are
 * benefits and costs to this approach. One of the main benefits is an easier
 * API for the programmer to manipulate complex stylings and properties. Another benefit is the built-in ability to
 * track whether a value has changed and whether it should be passed on to child elements. It also provides a sort of
 * abstract concept where the properties are the definition of how a given object is rendered.
 */

type PropertyInheritanceLevel = number

type PropStore = {
  // Actual, canonical value of the property
  value: any,
  // Whether the property has changed, as a bit mask. The last bit determines whether the real value has changed. The
  // second-to-last bit determines whether the user value has changed. The third-to-last bit determines whether the
  // program value has changed.
  changed: number,
  version?: number,
  inherit?: PropertyInheritanceLevel,
  userValue?: any,
  programValue?: any
}

interface PropsProxy {
  [key: string]: any
}

type PropertyConfigurationOptions = {
  inherit?: boolean
}

// See Props description for more
type PropertyInheritanceChanged = number
// For each property, types of values which may be accessed
type AccessorName = "default" | "real" | "user" | "program"
// Compact representation of an accessor type. 0 is real, 1 is user, 2 is program
type AccessorNameBit = 0 | 1 | 2

export class Props {
  /**
   * A key-object dictionary containing the values. The keys are the property names and the objects are of the form
   * { value, changed, ... some other metadata for the given property ... }.
   */
  store: Map<string, PropStore>
  // Just for fun... not sure if I'll keep this. Makes programming a bit less painful
  proxy: PropsProxy
  // Stores whether any property has changed as a bitmask
  hasChangedProperties: number
  // 0 when no inheritable properties have changed, 1 when an inheritable property has changed since the last time
  // the scene was fully updated, and 2 when the actual list of inheritable properties has changed (different
  // signature of inheritance, if you will).
  hasChangedInheritableProperties: PropertyInheritanceChanged

  constructor () {
    this.store = new Map()

    this.proxy = new Proxy(this, proxyHandlers)
    this.hasChangedProperties = 0
    this.hasChangedInheritableProperties = 0
  }

  static toBit (as: AccessorName): AccessorNameBit {
    switch (as) {
      case 'program':
        return 2
      case 'user':
        return 1
      case 'real':
      case 'default':
        return 0
    }

    throw new Error(`Unknown accessor name ${as}`)
  }

  _getPropertyStore (propName: string): PropStore | undefined {
    return this.store.get(propName)
  }

  _setPropertyStore (propName: string, value: PropStore) {
    this.store.set(propName, value)
  }

  /**
   * Create a property store for a given prop, returning the store. It returns the already-existing store, if appropriate.
   * @param propName
   * @returns Property store associated with the given property name
   */
  _createPropertyStore (propName: string): PropStore {
    let existing = this._getPropertyStore(propName)

    if (!existing) {
      existing = { value: undefined, changed: 0 }
      this._setPropertyStore(propName, existing)
    }

    return existing
  }

  /**
   * Deletes a property store wholesale, not trying to account for changed values and the like.
   * @param propName
   */
  _deletePropertyStore (propName: string) {
    this.store.delete(propName)
  }

  _forEachStore (callback: (store: PropStore) => void) {
    for (let value of this.store.values()) {
      callback(value)
    }
  }

  /**
   * Call the function with the name and (real) value of each property
   * @param callback
   */
  forEachProperty (callback: (propName: string, value: any) => void) {
    for (let [key, value] of this.store.entries()) {
      callback(key, value)
    }
  }

  /**
   * Get a list of all properties, including ones which are undefined but still have a store
   */
  listProperties (): string[] {
    return Array.from(this.store.keys())
  }

  /**
   * Returns whether a property has changed relative to the last marked update
   * @param propName
   */
  hasChanged (propName: string): boolean {
    return !!this._getPropertyStore(propName)?.changed
  }

  /**
   * Returns whether any property of a list of properties has changed, locally speaking.
   * @param propList
   */
  haveChanged (propList: string[]): boolean {
    return (
      !!this.hasChangedProperties && propList.some(prop => this.hasChanged(prop))
    )
  }

  /**
   * Returns whether a given property is inheritable (i.e., an inherit value of 1 or 2).
   * @param propName
   * @returns
   */
  isPropertyInheritable (propName) {
    return !!this._getPropertyStore(propName)?.inherit
  }

  /**
   * Returns a list of properties which have changed, locally speaking.
   */
  listChangedProperties () {
    return this.listProperties().filter(prop => this.hasChanged(prop))
  }

  /**
   * Returns a list of properties which are to be inherited (i.e., an inherit of 1 or 2).
   */
  listInheritableProperties (): string[] {
    return this.listProperties().filter(prop =>
      this.isPropertyInheritable(prop)
    )
  }

  /**
   * Inherit all inheritable properties from a given props. The function does this by comparing the local inherited
   * prop's version to the given props's version. If the local version is lower, the property and version are copied,
   * and the changed status is set to true. If updateAll is set to true, the function makes sure to check that the
   * actual list of inherited properties is synchronized, because it normally only checks the local inheritable
   * properties and compares them. In fact, it only checks the local inheritable properties with inherit: 1, which
   * indicates it came from a parent rather than being defined in the current element.
   * @param props {Props}
   * @param updateAll {boolean} Whether to force a complete update, in which the inheritable properties are verifiably
   * synced with the top element's properties. This usually happens after an element is added to a group, or after a
   * group's inheritance signature has changed.
   */
  inheritPropertiesFrom (props: Props, updateAll = false) {
    // Early exit condition, where if no inheritable properties have changed, we need not do anything
    if (!(updateAll || props.hasChangedInheritableProperties)) return

    updateAll = updateAll || props.hasChangedInheritableProperties === 2

    // We recalculate all local properties whose inheritance is 1, indicating they were inherited from above. Properties
    // not found above are deleted, properties found above are copied if their version is greater than or equal to the
    // version of the current property. This ensures that this props does not have any extraneous properties or any
    // incorrect/nonupdated values.
    for (const [propName, propStore] of this.store.entries()) {
      if (propStore.inherit !== 1) continue

      const otherPropsStore = props._getPropertyStore(propName)

      // if no such inheritable property, delete the local property (do not keep it as inheritable)
      if (
        !otherPropsStore ||
        otherPropsStore.inherit < 1 ||
        otherPropsStore.value === undefined
      ) {
        propStore.value = undefined
        propStore.changed |= 0b1
        propStore.inherit = 0

        this.markHasChangedProperties()
        this.markHasChangedInheritableProperties()
      }

      // Value has been changed!
      if (propStore.version === undefined || otherPropsStore.version > propStore.version) {
        propStore.version = otherPropsStore.version
        propStore.value = otherPropsStore.value
        propStore.changed |= 0b1

        this.markHasChangedProperties()
        this.markHasChangedInheritableProperties()
      }
    }

    // If updateAll is true, we run through all the given properties and inherit all 1s and 2s.
    if (updateAll) {
      for (const [propName, propStore] of props.store.entries()) {
        if (!propStore.inherit || propStore.value === undefined) continue

        let ourPropStore = this._getPropertyStore(propName)

        // Where things are actually inherited!!
        if (
          !ourPropStore ||
          (ourPropStore.inherit === 1 &&
            (ourPropStore.version === undefined || propStore.version > ourPropStore.version))
        ) {
          if (!ourPropStore) {
            ourPropStore = this._createPropertyStore(propName)

            // Goes around set
            ourPropStore.inherit = 1
            ourPropStore.value = propStore.value

            this.markHasChangedInheritanceSignature()
          }

          ourPropStore.version = propStore.version
          ourPropStore.value = propStore.value
          ourPropStore.changed |= 0b1

          this.markHasChangedProperties()
        }
      }
    }
  }

  /**
   * This function sets the value of a property. It is meant mostly for internal use. If prompted, it will check to see
   * whether the value given and the current value are strictly equal, or deeply equal, and if so, not mark the property
   * as changed. By default, this check is turned off, meaning all value assignments are marked as "changed". The third
   * parameter indicates whether the value should be directly modified, or
   * @param propName {string} The name of the property
   * @param value {any} The value of the property
   * @param as {number} Which value to change. 0 if real, 1 if user, 2 if program
   * @param equalityCheck {number} What type of equality check to perform against the current value, if any, to assess
   * the changed value. 0 - no check, 1 - strict equals, 2 - deep equals, 3 - deep equals, looking for "equals()" methods
   * @param markChanged {boolean} Whether to actually mark the value as changed. In turn, if the property is a changed
   * inheritable property, that will be noted
   * @returns {any}
   */
  set (propName: string, value: any, as: AccessorNameBit = 0, equalityCheck = 0, markChanged = true): any {
    let store = this._getPropertyStore(propName)

    // Helper functions to abstract away the "user/program/real" concept
    function getStoreValue (): any {
      store = store!
      switch (as) {
        case 0:
          return store.value
        case 1:
          return store.userValue
        case 2:
          return store.programValue
      }
    }

    function _setStoreValue (v) {
      store = store!
      switch (as) {
        case 0:
          store.value = v
          break
        case 1:
          store.userValue = v
          break
        case 2:
          store.programValue = v
          break
      }
    }

    if (value === undefined) {
      // Special case of deletion. If the property exists, we set its value to undefined, and if that property is
      // defined to be inheritable, we set this.hasChangedInheritableProperties to 2. Note that an inherited property
      // cannot be deleted, as that would be inconsistent; it can only be overridden.

      // trivial case, don't do anything
      if (!store || getStoreValue() === undefined) return value

      if (store.inherit === 1) {
        // If the store has an inheritance value of 1, we don't do anything
        return value
      } else if (store.inherit === 2) {
        // If the property has inheritance 2, we keep it as undefined and notify that the signature of inheritable properties has
        // changed.
        _setStoreValue(undefined)

        // If setting the real value, need to change the version
        if (as === 0) {
          store.version = getVersionID()
          if (markChanged) this.markHasChangedInheritanceSignature()
        }
      } else {
        // Set its value to undefined
        _setStoreValue(undefined)
      }

      if (markChanged) {
        // Mark which bit has changed
        store.changed |= 1 << as
        this.hasChangedProperties |= 1 << as
      }

      return undefined
    }

    // Otherwise, we need to get a property store
    if (!store) store = this._createPropertyStore(propName)

    // We reject assignments to an inherited property. This can be overridden by setting the property's inheritance
    // status.
    if (store.inherit === 1) return value

    if (equalityCheck !== 0) {
      let storeValue = getStoreValue()

      // Perform various equality checks
      if (equalityCheck === 1 && storeValue === value) return value
      else if (equalityCheck > 1 && deepEquals(storeValue, value, equalityCheck === 3))
        return value
    }

    // Set the value and changed values
    _setStoreValue(value)

    if (markChanged) {
      store.changed |= 1 << as
      this.hasChangedProperties |= 1 << as

      // For values to be inherited, store the version of this value. Only for inherit: 2 properties
      if (store.inherit === 2 && as === 0) {
        store.version = getVersionID()
        this.markHasChangedInheritableProperties()
      }
    }

    return value
  }

  markHasChangedProperties () {
    this.hasChangedProperties |= 0b1
  }

  markHasChangedInheritableProperties () {
    let c = this.hasChangedInheritableProperties
    this.hasChangedInheritableProperties = (c > 1) ? c : 1
  }

  markHasChangedInheritanceSignature () {
    this.hasChangedInheritableProperties = 2
  }

  configureProperty (propName, opts: PropertyConfigurationOptions = {}) {
    if (opts.inherit !== undefined) {
      this.setPropertyInheritance(propName, opts.inherit)
    }
  }

  configureProperties (propNames, opts = {}) {
    for (const propName of propNames) this.configureProperty(propName, opts)
  }

  /**
   * Set a property's inheritance to 2 (if inherit is true) or 0
   * @param propName {string}
   * @param inherit {boolean}
   * @return {Props}
   */
  setPropertyInheritance (propName: string, inherit = false) {
    // Force a property store to be created with undefined value
    const store = this._createPropertyStore(propName)

    let currentInheritance = !!store.inherit
    if (currentInheritance === inherit) return this

    if (inherit) {
      store.version = getVersionID()
      store.inherit = 2
    } else {
      delete store.version
      delete store.inherit
    }

    if (store.value !== undefined) this.hasChangedInheritableProperties = 2

    return this
  }

  /**
   * Get the value of a property.
   * @param propName {string}
   * @param as {number} 0 if getting the real value, 1 if getting the user value, 2 if getting the program value
   * @returns {*}
   */
  get (propName, as: AccessorNameBit = 0) {
    let store = this._getPropertyStore(propName)

    if (!store) return undefined
    switch (as) {
      case 0:
        return store.value
      case 1:
        return store.userValue
      case 2:
        return store.programValue
    }
  }

  getUserValue (propName: string): any {
    return this.get(propName, 1)
  }

  getProgramValue (propName: string): any {
    return this.get(propName, 2)
  }

  /**
   * Get the values of a list of properties.
   * @param propNameList
   */
  getProperties (propNameList: string[]): any[] {
    return propNameList.map(propName => this.get(propName))
  }

  /**
   * Mark all properties as locally updated (changed = false).
   */
  markAllUpdated (bitmask = 0b111): void {
    bitmask = ~bitmask
    this.hasChangedProperties &= bitmask

    this._forEachStore(store => {
      store.changed &= bitmask
    })
  }

  /**
   * Mark a specific property as locally updated (changed = false).
   * @param propName
   */
  markPropertyUpdated (propName: string) {
    const store = this._getPropertyStore(propName)

    if (store) store.changed = 0
  }

  /**
   * Mark a given property (if it exists) as changed.
   * @param propName {string}
   */
  markChanged (propName: string) {
    let store = this._getPropertyStore(propName)

    if (!store) return

    store.changed |= 0b1
    this.hasChangedProperties |= 0b1

    // If the store is inheritable, we need to generate a version ID
    if (store.inherit) {
      store.version = getVersionID()
      this.markHasChangedInheritableProperties()
    }
  }

  /**
   * Mark that no more inheritance is necessary. This function should only be called by the scene
   */
  _markGlobalUpdateComplete () {
    if (this.hasChangedProperties) this.markAllUpdated()
    this.hasChangedInheritableProperties = 0
  }

  stringify () {
    const obj = {}

    for (const [propName, propStore] of this.store) {
      obj[propName] = propStore
    }

    console.log(JSON.stringify(obj, null, 4))
  }
}

const proxyHandlers = {
  get: (target, propName) => {
    return target.get(propName)
  },
  set: (target, propName, value) => {
    target.set(propName, value)
    return true
  }
}
