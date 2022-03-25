import {EvaluationMode} from "./eval_modes.js";

type ConcreteTypeParams = {
  // Required
  name: string
  init: Function
  castPermissive: Function

  // "Optional"
  isPrimitive?: boolean
  initStr?: string | null
  typecheck?: Function | null
  typecheckVerbose?: Function | null
  isDefined?: Function | null
  clone?: Function | null
  copyTo?: Function | null
}

// Abstraction of a concrete type--an actual class of JS object or primitive used in calculations. For example, a bool
// and a number are both primitives, while a Grapheme.FastRealInterval is an object. Both are concrete types, although
// a number and FastRealinterval may both represent the abstract type "real". (See below)
export class ConcreteType {
  name: string
  isPrimitive: boolean
  init: Function
  defaultValue: any
  castPermissive: Function
  initStr: string | null

  typecheck: Function | null
  typecheckVerbose: Function | null
  isDefined: Function | null
  clone: Function | null
  copyTo: Function | null

  constructor (params: ConcreteTypeParams) {
    let name = this.name = params.name

    // Whether the type is a primitive, and thus whether it can be "written to"
    this.isPrimitive = !!params.isPrimitive

    // Function which, when called, returns a new instance of the type
    if (!params.init) {
      throw new TypeError(`ConcreteType ${name} needs an initializer`)
    }

    this.init = params.init

    if (!params.castPermissive) {
      throw new TypeError(`ConcreteType ${name} needs a permissive casting function`)
    }

    // FUNCTION which, when called with a single argument, attempts to construct the type and will never fail (will
    // returned the undefined version instead)
    this.castPermissive = params.castPermissive ?? null

    // Default value
    this.defaultValue = this.init()

    // STRING which, when eval-ed, returns a new instance of the type (used for primitives only)
    this.initStr = params.initStr ?? null

    // Returns true if the passed parameter is of this type
    this.typecheck = params.typecheck ?? null

    // Returns a verbose error message if the passed parameter is not of this type, otherwise an empty string
    this.typecheckVerbose = params.typecheckVerbose ?? null

    // Returns true if the passed parameter is considered defined. For example, Complex(0, NaN) would give false
    this.isDefined = params.isDefined ?? null

    // FUNCTION which, when called with a single argument, deep clones the type. Only used for non-primitives
    this.clone = params.clone ?? (x => x)

    // FUNCTION which, when called with two arguments src and dst, deep copies the contents of src to dst. Only used
    // for non-primitives
    this.copyTo = params.copyTo ?? null

    this.fillDefaults()
  }

  // Convenience method to avoid duplicate code for primitives
  fillDefaults () {
    if (this.isPrimitive) {
      if (!this.init || !this.initStr) {
        let init, initStr

        if (typeof this.defaultValue === "number") { // nullable booleans, ints, and reals are all represented by a JS number
          init = () => 0;
          initStr = "0";
        } else {
          throw new Error("Invalid primitive")
        }

        this.init = init
        this.initStr = initStr
      }
    }
  }

  isSameConcreteType (concreteType: ConcreteType) {
    return this.name === concreteType.name
  }

  toHashStr () {
    return this.name
  }

  prettyPrint () {
    return this.name
  }
}

type MathematicalTypeParams = {
  name: string
}

// Abstraction of a type. An expression or subexpression has a type. In the future we might want generics, so we use a
// class instead of a string. Note the distinction between this "type" and the concrete types used underneath. For
// example, "complex" is a type, while the Grapheme.Complex class is a concrete type. The unqualified word "type" refers
// to the former; "concrete type" is used in the latter case.
export class MathematicalType {
  name: string

  constructor (params: MathematicalTypeParams) {
    // Name of the type
    this.name = params.name
  }

  isSameType (type: MathematicalType) {
    return this.name === type.name
  }

  // Used for dictionary lookups (may change l8r)
  toHashStr () {
    return this.name
  }

  prettyPrint () {
    return this.name
  }
}
