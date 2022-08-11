
// The actual types and typecasts used by Grapheme.

// CONCRETE TYPES

import { ConcreteType, MathematicalType } from '../type.js'
import { NullableBoolean } from '../../bool/normal.js'
import { FastBooleanInterval } from '../../bool/fast_interval.js'
import { NullableInteger } from '../../int/normal.js'
import { RealInterval } from '../../real/interval.js'
import { Complex } from '../../complex/normal.js'
import { Vec2 } from "../../vec/vec2.js"

const concreteTypes = new Map<string, ConcreteType>()
const mathematicalTypes = new Map<string, MathematicalType>()

// The boolean type is nullable. meaning it takes on a value of 0, 1, or NaN. -0, false, and true are also ACCEPTED as
// values, but aren't used that way internally, since each is implicitly casted to the correct value in all numeric
// calculations.
let concreteBoolean = new ConcreteType({
  name: "bool",
  isPrimitive: true,
  init: () => 0,

  // The typechecks are for usability, not strictness
  typecheck: NullableBoolean.isUsableNullableBoolean,
  typecheckVerbose: NullableBoolean.typecheckUsableNullableBoolean,
  castPermissive: x => !!x
})

// Integers can be any number that's not a non-integral finite number (so they can be ±Infinity, NaN) but they can
// overflow--meaning any operation that takes them out of the [-2^53 - 1, 2^53 - 1] safe range
let concreteInt = new ConcreteType({
  name: "int",
  isPrimitive: true,
  init: () => 0,

  typecheck: NullableInteger.isNullableInteger,
  typecheckVerbose: NullableInteger.typecheckNullableInteger,
  castPermissive: x => Math.round(x)
})

// Real can be ANY floating-point number
let concreteReal = new ConcreteType({
  name: "real",
  isPrimitive: true,
  init: () => 0,

  typecheck: b => typeof b === "number",
  typecheckVerbose: b => (typeof b !== "number") ? ("Expected JS number, found type " + (typeof b)) : "",
  castPermissive: x => +x
})

let concreteComplex = new ConcreteType({
  name: "complex",
  isPrimitive: false,
  init: () => new Complex(0, 0),

  typecheck: b => b instanceof Complex,
  typecheckVerbose: b => (b instanceof Complex) ? "Expected complex number" : "",
  clone: c => new Complex(c.re, c.im),
  copyTo: (src, dst) => { dst.re = src.re; dst.im = src.im; },
  castPermissive: Complex.fromObj
})

// Strings are not implemented in interval arithmetic because that's rather meaningless
let concreteString = new ConcreteType({
  name: "string",
  isPrimitive: true,
  init: () => "",

  typecheck: b => typeof b === "string",
  typecheckVerbose: b => (typeof b !== "string") ? ("Expected JS string, found type " + (typeof b)) : "",
  castPermissive: x => x + ''
})

let concreteVec2 = new ConcreteType({
  name: "vec2",
  isPrimitive: false,
  init: () => new Vec2(0, 0),

  typecheck: b => b instanceof Vec2,
  typecheckVerbose: b => (b instanceof Vec2) ? "Expected Vec2" : "",
  clone: (c: Vec2) => new Vec2(c.x, c.y),
  copyTo: (src: Vec2, dst: Vec2) => { dst.x = src.x; dst.y = src.y },
  castPermissive: Vec2.fromObj
})

let concreteIntervalBoolean = new ConcreteType({
  name: "interval_bool",
  isPrimitive: false,
  init: () => new FastBooleanInterval(false, false, 0b111),

  typecheck: b => b instanceof FastBooleanInterval,
  clone: b => new FastBooleanInterval(b.min, b.max, b.info),
  copyTo: (src, dst) => { dst.min = src.min; dst.max = src.max; dst.info = src.info },
  castPermissive: () => { throw 1 }
})

let concreteIntervalReal = new ConcreteType({
  name: "interval_real",
  isPrimitive: false,
  init: () => new RealInterval(0, 0, 0b1111),

  typecheck: b => b instanceof RealInterval,
  clone: b => new RealInterval(b.min, b.max, b.info),
  copyTo: (src, dst) => { dst.min = src.min; dst.max = src.max; dst.info = src.info },
  castPermissive: RealInterval.fromObj
})

let concreteIntervalInt = new ConcreteType({
  ...concreteIntervalReal,
  name: "interval_int"
})

/**
 * MATHEMATICAL TYPES
 */

;[{
  name: "real"
}, {
  name: "int"
}, {
  name: "complex"
}, {
  name: "vec2"
}, {
  name: "vec3"
}, {
  name: "bool"
}, {
  name: "string"
}].forEach(params => defineMathematicalType(new MathematicalType(params)))


export function defineConcreteType (concreteType: ConcreteType) {
  let { name } = concreteType

  concreteTypes.set(name, concreteType)
}

export function defineMathematicalType (type: MathematicalType) {
  let { name } = type

  mathematicalTypes.set(name, type)
}

/**
 * "Intelligently" convert an object to the corresponding concrete type object. Returns null if no such type is found
 * @param o {any}
 * @param throwOnError
 * @returns {ConcreteType|null}
 */
export function toConcreteType (o, throwOnError=false): ConcreteType | null {
  let r: ConcreteType | null = null
  if (typeof o === "string") r = concreteTypes.get(o) ?? null
  else r = (o instanceof ConcreteType) ? o : null

  if (!r && throwOnError) {
    throw new Error("No concrete type found for " + o)
  }

  return r
}

/**
 * "Intelligently" convert an object to the corresponding mathematical type object
 * @param o {*}
 * @param throwOnError
 * @returns {MathematicalType|null}
 */
export function toMathematicalType (o, throwOnError=false): MathematicalType | null {
  let r: MathematicalType | null = null
  if (typeof o === "string") r = mathematicalTypes.get(o) ?? null
  else r = (o instanceof MathematicalType) ? o : null

  if (!r && throwOnError) {
    throw new Error("No mathematical type found for " + o)
  }

  return r
}

  // Concrete types
;[concreteBoolean, concreteInt, concreteReal, concreteIntervalBoolean, concreteIntervalInt, concreteIntervalReal, concreteComplex].forEach(defineConcreteType)

const ScopeTypeInit = () => {
  throw new Error("?")
}

/**
 * "Formal" concrete type representing a scope input into a compiled function
 */
export class ScopeType extends ConcreteType {
  scopeDict: [string: ConcreteType]

  constructor(scopeDict) {
    super({
      name: "scope",
      init: ScopeTypeInit,
      castPermissive: ScopeTypeInit
    })

    this.scopeDict = scopeDict
  }
}
