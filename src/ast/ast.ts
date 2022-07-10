
import {resolveOperatorDefinition} from './builtin/builtin_operators.js'
import {toMathematicalType} from "./builtin/builtin_types.js"
import {MathematicalConstants} from "./builtin/globals.js"
import {localWarn} from "../utils.js";
import {MathematicalType} from "./type.js";
import {MathematicalCast, OperatorDefinition} from "./operator_definition.js";
import { KeywordOperatorName } from "./parse";

import {toConcreteType} from "./builtin/builtin_types.js"
import {ConcreteType, MathematicalType} from "./type.js";


type ConcreteTypeParams = {
  // Required
  name: string
  init: () => any
  castPermissive: (any) => any

  // "Optional"
  isPrimitive?: boolean
  initStr?: string | null
  typecheck?: ((o: any) => boolean) | null
  typecheckVerbose?: ((o: any) => string) | null
  isDefined?: ((o: any) => boolean) | null
  clone?: ((o: any) => any) | null
  copyTo?: ((src: any, dst: any) => void) | null
}

// Abstraction of a concrete type--an actual class of JS object or primitive used in calculations. For example, a bool
// and a number are both primitives, while a Grapheme.FastRealInterval is an object. Both are concrete types, although
// a number and FastRealinterval may both represent the abstract type "real". (See below)
export class ConcreteType {
  name: string
  isPrimitive: boolean
  init: () => any
  defaultValue: any
  castPermissive: (any) => any
  initStr: string | null

  typecheck: ((o: any) => boolean) | null
  typecheckVerbose: ((o: any) => string) | null
  isDefined: ((o: any) => boolean) | null
  clone: ((o: any) => any) | null
  copyTo: ((src: any, dst: any) => void) | null

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


// The actual types and typecasts used by Grapheme.


// CONCRETE TYPES

import { ConcreteType, MathematicalType } from '../type.js'
import { NullableBoolean } from '../bool/normal.js'
import { FastBooleanInterval } from '../bool/fast_interval.js'
import { NullableInteger } from '../int/normal.js'
import { RealInterval } from '../real/interval.js'
import { Complex } from '../complex/normal.js'
import { Vec2 } from "../vec/vec2";

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

let concreteTypes = new Map<string, ConcreteType>()
let mathematicalTypes = new Map<string, MathematicalType>()

  // Concrete types
;[concreteBoolean, concreteInt, concreteReal, concreteIntervalBoolean, concreteIntervalInt, concreteIntervalReal, concreteComplex].forEach(defineConcreteType)


type AcceptableTypeMap = Map<string, ConcreteType> | { [key: string]: string }
type EvaluationModeParams = {
  args?: any[]  // default is []
  typeMap: AcceptableTypeMap
}

export class EvaluationMode {
  name: string
  args: any[]
  argCount: number
  typeMap: Map<string, ConcreteType>

  constructor (name: string, params: EvaluationModeParams) {
    this.name = name

    this.args = params.args ?? []
    this.argCount = this.args.length

    /**
     * Mapping between mathematical type hash strings and their concrete types
     * @type {Map<string, ConcreteType>}
     */
    this.typeMap = new Map()

    this.fillTypeMap(params.typeMap)
  }

  fillTypeMap (m: AcceptableTypeMap) {
    for (let [ mathematical, concrete ] of Object.entries(m)) {
      this.typeMap.set(mathematical, toConcreteType(concrete, true)!)
    }
  }

  /**
   * Get the concrete type associated with the mathematical type. Returns null if no type was found.
   */
  getConcreteType (mType: MathematicalType): ConcreteType | null {
    return this.typeMap.get(mType.name) ?? null
  }

  toString (): string { // for convenience
    return this.name
  }
}

const normal = new EvaluationMode("normal", {
  typeMap: {
    "int": "int",
    "real": "real",
    "bool": "bool",
    "complex": "complex"
  }
})

const fastInterval = new EvaluationMode("fast_interval", {
  typeMap: {
    "int": "interval_int",
    "real": "interval_real",
    "bool": "interval_bool"
  }
})

export const EvaluationModes: Map<string, EvaluationMode> = new Map()
EvaluationModes.set("normal", normal)
EvaluationModes.set("fast_interval", fastInterval)

/**
 * Convert the argument, either a string or an evaluation mode, to the corresponding evaluation mode
 * @param o Any object
 * @param throwOnError Whether to throw a descriptive error
 */
export function toEvaluationMode(o: any, throwOnError=true): EvaluationMode | null {
  if (o instanceof EvaluationMode) return o
  let mode = EvaluationModes.get(o) ?? null

  if (!mode && throwOnError) {
    if (typeof o === "string") {
      throw new Error(`Unrecognized evaluation mode '${o}'`)
    } else {
      throw new Error("Evaluation mode must be a string (e.g., 'normal') or EvaluationMode object")
    }
  }

  return mode
}

/**
 * To evaluate a given node whose operators and types have been identified, we provide the following:
 *
 * An evaluation mode, which may be a string;
 * A scope, which is a key-value pair of variable values that will be casted to the corresponding mode type;
 * An option value.
 *
 * Evaluating a constant node:
 *   - Identify the correct concrete type
 *   - Cast the value to the concrete type
 *   - Return
 *
 * Evaluating an ASTGroup:
 *   - Return the result of the first child node
 *
 * Evaluating an OperatorNode:
 *   - Compute the values of all children
 *   - Look up the correct concrete evaluator
 *   - Look up the correct concrete casts
 *   - Cast the values of all children to the correct types
 *   - Call the concrete evaluator. extraArgs is passed as the last parameter if it is not empty.
 *   - Return
 *
 * Evaluating a VariableNode:
 *   - Identify the correct concrete type
 *   - Cast the value in the provided scope to the concrete type
 *   - Return
 */

class EvaluationError extends Error {
  constructor (message) {
    super(message)

    this.name = 'EvaluationError'
  }
}

class ResolutionError extends Error {
  constructor (message) {
    super(message)

    this.name = 'ResolutionError'
  }
}

const reservedVariableNames = [
  // Properties of the default object
  "constructor",
  "hasOwnProperty",
  "isPrototypeOf",
  "propertyIsEnumerable",
  "toLocaleString",
  "toString",
  "valueOf",

  "scope",
  "scope_map"
]

/**
 * Helper function (doesn't need to be fast)
 * @returns {string}
 */
function prettyPrintNode(node: ASTNode, name: string, keys: string[], params: any) {
  let out: string[] = []

  for (let key of keys) {
    let value = node[key]

    if (value != null) {
      if (key === "children") // Get children, pretty printed
        value = "List{" + value.map(node => node.prettyPrint(params)).join(", ") + "}"
      if (key === "name") // surround with quotes
        value = `"${value}"`
      if (key === "type")
        value = value.toHashStr()

      out.push(`${key}=${value}`)
    }
  }

  return name + "{" + out.join(", ") + "}"
}

const KNOWN_KEYS = ["type", "value", "name", "children"]
const suspiciousVariableNames = [
  "defaultType",
  "throwOnUnresolved"
]

type ASTNodeInfo = {
  extraArgs?: any
  isFunction?: boolean
}

export type ASTNodeParams = {
  type?: MathematicalType | null
  info?: ASTNodeInfo | null
  operatorDefinition?: OperatorDefinition | null
}

type ASTNodeFunctor = (node: ASTNode, depth?: number) => void

type ResolveTypesOptions = {
  /** @defaultValue "real" */
  defaultType?: string | MathematicalType
  /** @defaultValue true */
  throwOnUnresolved?: boolean
}

type FilledResolveTypesOptions = {
  defaultType: MathematicalType
  throwOnUnresolved: boolean
  vars: { [key: string]: MathematicalType }
}

type VariableDependency = {
  type: MathematicalType
  operatorDefinition: null | OperatorDefinition
  count: number
}

type VariableDependencies = Map<string, VariableDependency>

// Dictionary between variable names and their values
type VariableLookupObject = {
  [key: string]: any
}

type EvaluationOptions = {

}

type ResolveTypesVariableInfo = {
  [key: string]: string | MathematicalType
}

type NodeTypeEnum = 0 | 1 | 2 | 3 | 4

type ConvertToStringOptions = {
  elideParentheses?: boolean
}

type FilledConvertToStringOptions = {
  elideParentheses: boolean
}

type StringIntermediateResult = {
  isOnlyVariable: boolean
  lastOperator: KeywordOperatorName | string
  lastNodeType: NodeTypeEnum
  lastArity: 0 | 1 | 2
  contents: string
}

const DEFAULT_CONVERT_TO_STRING_OPTIONS: FilledConvertToStringOptions = {
  elideParentheses: true
}

class ASTNode {
  type: MathematicalType | null
  info: ASTNodeInfo
  operatorDefinition: OperatorDefinition | null
  topNode: any // TODO

  constructor (params: ASTNodeParams) {
    /**
     * MathematicalType of the node (int, complex, etc.). Null if not resolved
     */
    this.type = params.type ?? null

    /**
     * Other info about the node (for example, where it was in a parsed string)
     */
    this.info = params.info ?? {}

    /**
     * The node's operator. If a constant, this will be null and the value will be converted later. If an operator, this
     * must not be null (or the definition is not known). If a variable, this will be called if this is not null.
     */
    this.operatorDefinition = params.operatorDefinition ?? null

    /**
     * Highest node in this tree
     */
    this.topNode = null
  }

  /**
   * Apply a function f to all children recursively, with some options
   * @param f Function taking in an ASTNode as its first argument, and optionally the depth from the top node as its second
   * @param onlyGroups Only apply f on groups
   * @param childrenFirst Whether to call f on children first
   * @param depth Starting depth
   */
  applyAll (f: ASTNodeFunctor, onlyGroups=false, childrenFirst=false, depth=0) {
    if (!onlyGroups) f(this, depth)
  }

  /**
   * Whether this node is a group
   */
  isGroup (): boolean {
    return false
  }

  /**
   * Whether this node is an operator node that is semantically a function (and not an infix, prefix or postfix operator)
   */
  isFunctionNode (): boolean {
    return false
  }

  toString (): string {
    return `[object ${this.nodeTypeAsString()}]`
  }

  /**
   * Stringify this node by turning it into a readable string
   */
  toExprString(opts: ConvertToStringOptions = {}): string {
    let filledOpts = Object.assign({}, DEFAULT_CONVERT_TO_STRING_OPTIONS, opts) as FilledConvertToStringOptions

    let intermediate = this._toExprString(filledOpts as FilledConvertToStringOptions)

    return ""
  }

  _toExprString(opts: FilledConvertToStringOptions): StringIntermediateResult {
    return "poop" as any
  }

  /**
   * Node type as an enum (use nodeTypeAsString() for a string version)
   */
  nodeType (): NodeTypeEnum {
    return 0
  }

  /**
   * Get the node type as a string instead of an enum
   * @returns {string}
   */
  nodeTypeAsString () {
    switch (this.nodeType()) {
      case 0: return "ASTNode"
      case 1: return "ConstantNode"
      case 2: return "VariableNode"
      case 3: return "OperatorNode"
      case 4: return "ASTGroup"
    }

    return "UnknownNode"
  }

  /**
   * For debug use. Example: OperatorNode{type=int, name="+", children=List{ConstantNode{type=int, value="3"}, VariableNode{type=int, name="x"}}}
   */
  prettyPrint (params={}): string {
    return prettyPrintNode(this, this.nodeTypeAsString(), KNOWN_KEYS, params)
  }

  // Enum of node types
  static TYPES = Object.freeze({
    ASTNode: 0,
    ConstantNode: 1,
    VariableNode: 2,
    OperatorNode: 3,
    ASTGroup: 4
  } as const)

  /**
   * Deep clone this ASTNode TODO
   */
  clone (): ASTNode {
    return new ASTNode(this)
  }

  /**
   * Figure out the type of each node, given the type of each variable node within it. Examples:
   *
   * node = Grapheme.parseString("(x+y)^2+cos(z)").resolveTypes({ z: "real" }, { defaultType: "complex" })
   *    -> Node which takes in a real value z and two complex values x and y
   * node = Grapheme.parseString("(x+y)^2+cos(z)").resolveTypes({}, { defaultType: "complex" })
   *    -> Node which takes in three complex values
   * node = Grapheme.parseString("(x+y)^2+cos(z)").resolveTypes()
   *    -> Node which takes in three real values
   * node = Grapheme.parseString("cowbell(z)")    // (does not throw)
   * node.resolveTypes()                          // throws
   *
   * Perf: on "x^2+y^2+e^-x^2+pow(3,gamma(2401 + complex(2,3)))", took 0.002 ms / iteration as of Mar 14, 2022
   *
   * @param vars {{}} Mapping from variable names to their types
   * @param opts
   */
  resolveTypes (vars: ResolveTypesVariableInfo | undefined, opts: ResolveTypesOptions = {}): ASTNode {
    // Convert all arg values to mathematical types

    let { defaultType = "real", throwOnUnresolved = true } = opts
    vars ??= {}

    for (let sus of suspiciousVariableNames) {
      if (sus in vars) {
        localWarn(`Option ${sus} found in first argument to resolveTypes(vars, opts). Note that vars is a dictionary of variables, so ${sus} will be treated as a variable.`,
          `unusual variable name in resolveTypes()`, 3)
      }
    }

    let revisedVars: FilledResolveTypesOptions["vars"] = {}
    for (let v in vars) {
      if (!vars.hasOwnProperty(v)) continue
      let n = revisedVars[v]

      revisedVars[v] = toMathematicalType(n, true /* throw on error */)!
    }

    let revisedType = toMathematicalType(defaultType, true)!

    this.applyAll(node => node._resolveTypes({
      vars: revisedVars, throwOnUnresolved, defaultType: revisedType!
    }), false /* only groups */, true /* children first */)

    return this
  }

  /**
   * Whether all operator definitions and types have been resolved for this expression by checking whether the type of
   * this node is known. If it is not known, then at least one of the types of its children is not known either
   */
  allResolved(): boolean {
    return !!(this.type)
  }

  /**
   * Internal resolve types function recursively called
   */
  _resolveTypes (opts: FilledResolveTypesOptions) {

  }

  /**
   * Evaluate the function in place (without compilation), passing variable values as a dictionary. This is SLOW. Don't
   * call it unless you're only going to evaluate the function a handful of times
   * @param vars
   * @param opts
   */
  evaluate (vars, { mode = "normal", typecheck = true } = {}) {
    if (!this.allResolved())
      throw new EvaluationError(`[E0001] This node has not had its types fully resolved (call .resolveTypes()).\
In other words, the node cannot be used for computation until an abstract type and operator definition has been found for each node.`)

    let convertedMode = toEvaluationMode(mode ?? "normal", true) // throws on fail

    return this._evaluate(vars, convertedMode!, { mode, typecheck })
  }

  _evaluate (vars: VariableLookupObject, mode: EvaluationMode, opts: EvaluationOptions) {
    throw new EvaluationError("ASTNode cannot be evaluated directly")
  }

  /**
   * Returns a Map of variable names to information about those variables.
   */
  getVariableDependencies (): VariableDependencies {
    let knownVars: VariableDependencies = new Map()

    this.applyAll((node: ASTNode) => {
      if (node.nodeType() === ASTNode.TYPES.VariableNode) {
        let name = (node as VariableNode).name
        let info = knownVars.get(name)

        if (!info) {
          if (node.type == null) {
            throw new EvaluationError(`[E0001] Type of variable ${name} has not been resolved`)
          }

          info = {
            type: node.type,
            operatorDefinition: node.operatorDefinition,
            count: 1
          }

          knownVars.set(name, info)
        } else {
          info.count++
        }
      }
    })

    return knownVars
  }

  /**
   * Convert a node to LaTeX under a set of LaTeX options. LaTeX is done recursively
   */
  toLatex (opts: ConversionToLatexOptions = {}): string {
    opts = Object.assign({}, DefaultConversionToLatexOptions, opts)

    let filledOpts: FilledConversionToLatexOptions = {
      useDynamicDelimeters: cvtMaybeBooleanOpt(opts.useDynamicDelimeters) as FilledUseDynamicDelimeters,
      elideParentheses: cvtMaybeBooleanOpt(opts.elideParentheses) as FilledElideParentheses,
      elideMultiplication: cvtMaybeBooleanOpt(opts.elideMultiplication) as FilledElideMultiplication,
      multiplicationSymbol: opts.multiplicationSymbol as MultiplicationSymbol
    }

    let intermediate = this._toLatex(filledOpts)

    return intermediate.contents
  }

  /**
   * Internal conversion function
   * @param opts
   */
  _toLatex (opts: FilledConversionToLatexOptions): LatexIntermediateResult {
    return {
      isTall: false,
      contents: ""
    }
  }
}

function cvtMaybeBooleanOpt(opt: any): any {
  if (typeof opt === "boolean") {
    return opt ? "always" : "never"
  }

  return opt
}

type LatexIntermediateResult = {
  /**
   * Whether the result is considered "tall" for the purposes of dynamic delimeters
   */
  isTall: boolean,

  contents: string
}

type FilledUseDynamicDelimeters = "always" | "sometimes" | "never"

/**
 * Whether to always use \left and \right with parentheses, etc. Always/true means use these with every parenthesis.
 * Sometimes means use these with every parenthesis which is predicted to contain a "tall" element. Tall elements
 * include fractions,
 */
type UseDynamicDelimeters = FilledUseDynamicDelimeters | boolean

type FilledElideParentheses = "always" | "sometimes" | "never"

/**
 * Whether to remove parentheses which may be ignored by the usual rules of PEMDAS. If "sometimes", parentheses that are
 * unnecessary for disambiguation, EXCEPT for non-nested parentheses in vertical fractions and non-nested parentheses in
 * repeated exponentiation, will be kept. If "never", parentheses
 */
type ElideParentheses = FilledElideParentheses | boolean

type FilledElideMultiplication = "always" | "never"

type ElideMultiplication = FilledElideMultiplication | boolean

type MultiplicationSymbol = "cdot" | "times"

export type ConversionToLatexOptions = {
  useDynamicDelimeters?: UseDynamicDelimeters
  elideParentheses?: ElideParentheses
  multiplicationSymbol?: "cdot" | "times"
  elideMultiplication?: ElideMultiplication
}

const DefaultConversionToLatexOptions: FilledConversionToLatexOptions = {
  useDynamicDelimeters: "always",
  elideParentheses: "always",
  multiplicationSymbol: "cdot",
  elideMultiplication: "always"
}

type FilledConversionToLatexOptions = {
  useDynamicDelimeters: FilledUseDynamicDelimeters
  elideParentheses: FilledElideParentheses
  multiplicationSymbol: MultiplicationSymbol
  elideMultiplication: ElideMultiplication
}

type ASTGroupParams = ASTNodeParams & {
  children?: ASTNode[]
}

// Node with children. A plain ASTGroup is usually just a parenthesized thing
class ASTGroup extends ASTNode {
  children: ASTNode[]

  constructor (params: ASTGroupParams = {}) {
    super(params)

    this.children = params.children ?? []

    this.info.isFunction = false
  }

  applyAll (func, onlyGroups = false, childrenFirst = false, depth = 0) {
    if (!childrenFirst) func(this, depth)

    //let stack: Array<ASTNode> = [ this ]

    let children = this.children
    for (let i = 0; i < children.length; ++i) {
      let child = children[i]
      if (!onlyGroups || child.isGroup()) {
        child.applyAll(func, onlyGroups, childrenFirst, depth + 1)
      }
    }

    if (childrenFirst) func(this, depth)

    return this
  }

  isGroup () {
    return true
  }

  nodeType () {
    return 4 as NodeTypeEnum
  }

  clone () {
    return new ASTGroup(this)
  }

  _resolveTypes (opts: FilledResolveTypesOptions) {
    // Only called on a raw group, aka a parenthesized group
    let children = this.children

    for (let i = 0; i < children.length; ++i) {
      children[i]._resolveTypes(opts)
    }

    this.type = children[0].type
  }

  _evaluate (vars, mode, opts) {
    return this.children[0]._evaluate(vars, mode, opts)
  }

  __toLatex(opts) {
    // An ASTGroup is a purposefully parenthesed
  }
}

type ConstantNodeParams = ASTNodeParams & {
  value: string
  type: MathematicalType | null // type required for constants
}

class ConstantNode extends ASTNode {
  value: string

  constructor (params: ConstantNodeParams) {
    super(params)

    // Generally a text rendition of the constant node; e.g., "0.3" or "50"
    this.value = params.value
    if (!params.type) {
      throw new EvaluationError(`ConstantNode must be constructed with a mathematical type (e.g., real)`)
    }

    this.type = params.type
  }

  nodeType () {
    return 1 as NodeTypeEnum
  }

  clone () {
    return new ConstantNode(this)
  }

  _resolveTypes (args: FilledResolveTypesOptions) {

  }

  _evaluate (vars: VariableLookupObject, mode: EvaluationMode, opts: EvaluationOptions): any {
    if (!this.type) {
      throw new EvaluationError(`[E0001] Type of constant variable with value ${this.value} has not been resolved`)
    }

    let type = mode.getConcreteType(this.type)

    if (!type) {
      throw new EvaluationError(`Cannot find concrete type in mode ${mode.name} for mathematical type ${this.type.toHashStr()}`)
    }

    return type.castPermissive(this.value)
  }
}

type VariableNodeParams = ASTNodeParams & {
  name: string
  operatorDefinition?: OperatorDefinition | null
}

class VariableNode extends ASTNode {
  name: string
  operatorDefinition: OperatorDefinition | null

  constructor (params: VariableNodeParams) {
    super(params)

    this.name = params.name
    this.operatorDefinition = params.operatorDefinition ?? null
    // TODO var name check in parse string
  }

  nodeType () {
    return 2 as NodeTypeEnum
  }

  clone() {
    return new VariableNode(this)
  }

  _resolveTypes (opts: FilledResolveTypesOptions) {
    let { vars, defaultType } = opts

    let name = this.name

    let info
    if (vars)
      info = vars[this.name]

    if (!info && name in MathematicalConstants) { // pi, e, i
      let constant = MathematicalConstants[name] // OperatorDefinition

      info = constant.returns
      this.operatorDefinition = constant
    }

    this.type = toMathematicalType(info ?? (defaultType ?? "real"))
  }

  _evaluate (vars, mode, opts) {
    if (this.operatorDefinition) { // pi, e, i
      let evaluator = this.operatorDefinition.getDefaultEvaluator(mode)

      if (evaluator === null) {
        throw new EvaluationError(`No known definition for constant ${this.name} in mode ${mode.name}`)
      }

      return evaluator.callNew([])
    }

    let v = vars[this.name]
    if (v === undefined) {
      throw new EvaluationError(`Variable ${this.name} is not defined in the current scope`)
    }

    if (opts.typecheck) {
      let concreteType = mode.getConcreteType(this.type)

      let works = concreteType.typecheck(v)
      if (!works) {
        let msg = concreteType.typecheckVerbose?.(v) ?? ""

        throw new EvaluationError(`Variable ${this.name} should have concrete type ${concreteType.toHashStr()}. ${msg}`)
      }
    }

    return v
  }
}

type OperatorNodeParams = ASTGroupParams & {
  name: string
  extraArgs?: { [key: string]: string },
  casts: MathematicalCast[] | null
}

class OperatorNode extends ASTGroup {
  name: string
  casts: MathematicalCast[] | null  // null when casts are not known

  // Extra arguments that have an effect on the operator's mathematical meaning, but which are unwieldy to represent
  // directly as an argment. Current use: comparison chain, where the arguments are the comparisons to be done and
  // extraArgs.comparisons is, say, [ '<', '<=' ]
  extraArgs: { [key: string]: string }

  constructor (params: OperatorNodeParams) {
    super(params)

    this.name = params.name
    this.children = params.children ?? []
    this.extraArgs = params.extraArgs ?? {}
    this.casts = params.casts ?? []
  }

  nodeType () {
    return 3 as NodeTypeEnum
  }

  isFunctionNode() {
    return !!this.info.isFunction
  }

  clone () {
    return new OperatorNode(this)
  }

  childArgTypes (): (MathematicalType|null)[] {
    return this.children.map(c => c.type)
  }

  _resolveTypes (opts) {
    let childArgTypes = this.childArgTypes()

    if (!childArgTypes.some(t => t === null)) { // null check done
      let [ definition, casts ] = resolveOperatorDefinition(this.name, childArgTypes as MathematicalType[])

      if (definition !== null && casts!.every(cast => cast !== null)) {
        this.type = definition.returns
        this.operatorDefinition = definition
        this.casts = casts

        return
      }
    }

    this.type = null
    this.operatorDefinition = null
    this.casts = null

    if (opts.throwOnUnresolved) {
      throw new ResolutionError(`Unable to resolve operator definition ${this.name}(${childArgTypes.map(t => t?.toHashStr() ?? "unknown")})`)
    }
  }

  _evaluate (vars, mode, opts) {
    if (!this.operatorDefinition) throw new EvaluationError("Operator definition not resolved")
    if (!this.casts) throw new EvaluationError("Casts not resolved")

    let casts = this.casts
    let childrenValues: any[] = []
    let children = this.children
    let cl = children.length

    for (let i = 0; i < cl; ++i) {
      let c = children[i]
      let cast = casts[i]

      let ccast = cast.getDefaultEvaluator(mode)

      if (ccast === null) {
        throw new EvaluationError(
          `No concrete cast (in mode ${mode.name}) from source ${mode.getConcreteType(cast.srcType()).toHashStr()}`
          + ` to destination ${mode.getConcreteType(cast.dstType()).toHashStr()}`)
      }

      let uncastedChild = c._evaluate(vars, mode, opts)
      childrenValues.push(ccast.callNew([
        uncastedChild
      ]))
    }

    let evaluator = this.operatorDefinition.getDefaultEvaluator(mode)
    if (!evaluator) {
      throw new EvaluationError(
        `No evaluator (in mode ${mode.name}} for operator ${this.operatorDefinition.prettyPrint()}`
      )
    }

    return evaluator.callNew(childrenValues)
  }
}

/**
 * In this file, we convert strings representing expressions in Grapheme into their ASTNode counterparts. For example,
 * x^2 is compiled to OperatorNode{operator=^, children=[VariableNode{name="x"}, ConstantNode{value="2"}]}
 */
import {
  ConstantNode,
  VariableNode,
  OperatorNode,
  ASTGroup,
  ASTNode, ConstantNodeParams, ASTGroupParams, OperatorNodeParams, VariableNodeParams
} from './node.js'
import { toMathematicalType } from './builtin/builtin_types.js'
import {MathematicalType} from "./type.js";
import { isStringInteger } from "./is_string_integer.js"

export class ParserError extends Error {
  constructor (message) {
    super(message)

    this.name = 'ParserError'
  }
}

type BaseToken = {
  index: number
}

type ParenToken = BaseToken & {
  type: "paren"
  paren: '(' | ')' | '[' | ']'
}

type ConstantToken = BaseToken & {
  type: "constant"
  value: string
}

const KEYWORD_OPERATOR_NAMES = [
  '-' , '*' , '/' , '+' , 'and' , 'or' , '>=' , '>' , '<' , '<=' , '==' , '!=' , '^'
] as const

export type KeywordOperatorName = typeof KEYWORD_OPERATOR_NAMES[number]

export function isKeywordOperator(s: string): boolean {
  return KEYWORD_OPERATOR_NAMES.includes(s as any)
}

const PRECEDENCES = {
  '': 20,
  'cchain': 19,
  '^': 11,
  // unary minus is handled separately
  '*': 10,
  '/': 10,
  '+': 8,
  '-': 8,
}

export function getOperatorPrecedence(s: string, arity: number): number {
  // PFCUEMDAS, where the empty string is considered to be generic parentheses. P=F, U=E right to left, M=D, A=S. If the
  // returned precedence is odd, it should be considered right to left.
  if (arity !== 1 && arity !== 2) throw new RangeError("?")

  if (arity === 1) {
    return (s === '-') ? 11 : -2
  }

  let p: number | undefined = PRECEDENCES[s]

  return p ?? 20
}

type OperatorToken = BaseToken & {
  type: "operator"
  op: KeywordOperatorName
}

type FunctionToken = BaseToken & {
  type: "function"
  name: string
}

type CommaToken = BaseToken & {
  type: "comma"
}

type VariableToken = BaseToken & {
  type: "variable"
  name: string
}

type StringToken = BaseToken & {
  type: "string"
  contents: string
}

type Token = ParenToken | ConstantToken | OperatorToken | FunctionToken | CommaToken | VariableToken | StringToken

type ASTNodeInfo = {
  token?: Token
  startToken?: Token
  endToken?: Token

  // Operators
  isFunction?: boolean
  // Functions
  startExprToken?: Token

  // Constants
  value?: string
  type?: MathematicalType
  // Variables, operators, functions
  name?: string
  // Comparison chains
  comparisons?: string[]
  // For root nodes: which string this was generated from, if any
  parsedFrom?: string
}
type UnprocessedASTNodeType = "generic" | "constant" | "variable" | "operator" | "group"
type UnprocessedChild = UnprocessedASTNode | Token
type UnprocessedChildren = UnprocessedChild[]

// ASTNode that might still have tokens in it
class UnprocessedASTNode {
  type: "node"  // as opposed to a token
  nodeType: UnprocessedASTNodeType
  info: ASTNodeInfo
  children: UnprocessedChildren

  constructor (nodeType: UnprocessedASTNodeType, info: ASTNodeInfo, children: UnprocessedChildren) {
    this.nodeType = nodeType
    this.info = info
    this.children = children
    this.type = "node"
  }

  applyAll (f: (UnprocessedChild) => void, childrenFirst=false) {
    let children = this.children

    if (!childrenFirst) f(this)
    for (let i = 0; i < children.length; ++i) {
      let child = children[i]
      if (child instanceof UnprocessedASTNode) {
        child.applyAll(f, childrenFirst)
      } else {
        f(child)
      }
    }
    if (childrenFirst) f(this)
  }
}

type ParserErrorInfo = null | {
  index: number
} | {
  token: Token
  endToken?: Token
}

const operator_regex = /^[*\-\/+^]|^[<>]=?|^[=!]=|^and\s+|^or\s+/
const function_regex = /^([a-zA-Z_][a-zA-Z0-9_]*)\(/  // functions may only use (, [ is reserved for indexing
const constant_regex = /^[0-9]*\.?[0-9]*e?[0-9]+/
const variable_regex = /^[a-zA-Z_][a-zA-Z0-9_]*/
const paren_regex = /^[()\[\]]|\|\|?/
const comma_regex = /^,/
const string_regex = /^"(?:[^"\\]|\\.)*"/

/**
 * Helper function to throw an error at a specific index in a string.
 * TODO make signature uniform
 * @param string Erroneous parsed string
 * @param info The token in the string where the error occurred, ideally with an index attribute
 * @param message The raw error message, to be combined with contextual information
 * @param noIndex If true, provide no index
 * @param postScript Note to provide at end of message.
 */
function raiseParserError (string: string, info: ParserErrorInfo, message: string = "", noIndex: boolean =false, postScript: string=""): never {
  let index = -1, token: null | Token = null, endToken: null | Token = null

  if (info !== null) {
    if ('index' in info) {
      index = info.index
    } else {
      token = info.token
      if (info.endToken != null) {
        endToken = info.endToken
      }

      index = token.index
    }
  }

  if (!noIndex) // can't use an index if we have no index information
    noIndex = index === -1

  let spaces = noIndex ? '' : ' '.repeat(index)
  let errorLen = (endToken?.index ?? index) - index + 1

  throw new ParserError(
    'Malformed expression; ' + message +
    (noIndex ? '' : ' at index ' + index + ':\n' + string + '\n' + spaces + '^'.repeat(errorLen))
    + (postScript ? ('\nNote: ' + postScript) :"")
  )
}

function raiseUnknownParserError (): never {
  throw new ParserError("?")
}

function checkParensBalanced (s: string) {
  // TODO: Handle strings as tokens

  const parenStack: string[] = []

  let i = 0
  let err = false

  outer: for (; i < s.length; ++i) {
    let chr = s[i]

    switch (chr) {
      case '(':
      case '[':
        parenStack.push(chr)
        break
      case ')':
      case ']':
        if (parenStack.length === 0) {
          err = true
          break outer
        }

        if (chr === ')') {
          let pop = parenStack.pop()

          if (pop !== '(') {
            err = true
            break outer
          }
        } else {
          let pop = parenStack.pop()

          if (pop !== '[') {
            err = true
            break outer
          }
        }
    }
  }

  if (parenStack.length !== 0) err = true

  if (err) raiseParserError(s, { index: i }, 'unbalanced parentheses/brackets')
}

const trimRight = ('trimRight' in String.prototype) ? (s: string): string => (s as any).trimRight() : (s: string) => {
  return s.replace(/\s+$/, '')
}

function getTokens (s: string): Token[] {
  // TODO swtich to non-regex implementation
  let i = 0
  let original_string = s

  s = trimRight(s)
  let prev_len = s.length

  let tokens: Token[] = []

  while (s) {
    s = s.trim() // repeatedly trim off whitespace and grab the next token TODO: optimize to simply iterate

    i += prev_len - s.length
    prev_len = s.length

    let match

    do {
      match = s.match(paren_regex)

      if (match) {
        let paren = match[0]
        if (paren === '|' || paren === '||') {
          // Attempted to use unsupported parenthesis-like syntax
          raiseParserError(original_string, { index: i }, 'unrecognized token', false,
            'For absolute value, use abs(x) or mag(z), both of which accept complex values.')
          // TODO: For the norm of a vector, use norm(v). For the determinant of a matrix, use det(M).
        }

        tokens.push({
          type: 'paren',
          paren,
          index: i
        })

        break
      }

      match = s.match(constant_regex)

      if (match) {
        tokens.push({
          type: 'constant',
          value: match[0],
          index: i
        })

        break
      }

      match = s.match(operator_regex)

      if (match) {
        tokens.push({
          type: 'operator',
          op: match[0].replace(/\s+/g, ''),
          index: i
        })

        break
      }

      match = s.match(comma_regex)

      if (match) {
        tokens.push({
          type: 'comma',
          index: i
        })

        break
      }

      match = s.match(function_regex)

      if (match) {
        // First group is the function name, second group is the type of parenthesis (bracket or open)

        tokens.push({
          type: 'function',
          name: match[1],
          index: i
        })

        tokens.push({
          type: 'paren',
          paren: '(',
          index: i + match[1].length
        })

        break
      }

      match = s.match(variable_regex)

      if (match) {
        tokens.push({
          type: 'variable',
          name: match[0],
          index: i
        })

        break
      }

      match = s.match(string_regex)

      if (match) {
        tokens.push({
          type: 'string',
          contents: match[0].slice(1, -1),
          index: i
        })
      }

      raiseParserError(original_string, { index: i }, 'unrecognized token')
    } while (false)

    let len = match[0].length

    s = s.slice(len) // rm token
  }

  return tokens
}

function checkValid (tokens, string) {
  if (tokens.length === 0) {
    raiseParserError(string, { index: 0 }, 'empty expression', true)
  }

  for (let i = 0; i < tokens.length - 1; ++i) {
    let token1 = tokens[i]
    let token2 = tokens[i + 1]

    let token2IsUnary = token2.op === '-' || token2.op === '+'

    if (
      (token1.type === 'operator' || token1.type === 'comma') &&
      (token2.type === 'operator' || token2.type === 'comma') &&
      (!token2IsUnary || i === tokens.length - 2)
    ) {
      raiseParserError(string, token2, 'two consecutive operators')
    }
    if (token1.paren === '(' && token2.paren === ')')
      raiseParserError(string, token2, 'empty parentheses not associated with function call')
    if (token1.paren === '[' && token2.paren === ']')
      raiseParserError(string, token2, 'empty brackets not associated with function call')
    if (token1.type === 'operator' && token2.paren === ')')
      raiseParserError(
        string,
        token2,
        'operator followed by closing parenthesis'
      )
    if (token1.type === 'operator' && token2.paren === ']')
      raiseParserError(
        string,
        token2,
        'operator followed by closing bracket'
      )
    if (token1.type === 'comma' && token2.paren === ')')
      raiseParserError(
        string,
        token2,
        'comma followed by closing parenthesis'
      )
    if (token1.type === 'comma' && token2.paren === ']')
      raiseParserError(string, token2, 'comma followed by closing bracket')
    if (token1.paren === '(' && token2.type === 'comma')
      raiseParserError(string, token2, 'comma after open parenthesis')
    if (token1.paren === '[' && token2.type === 'comma')
      raiseParserError(string, token2, 'comma after starting bracket')
    if (token1.paren === '(' && token2.type === 'operator' && !token2IsUnary)
      raiseParserError(string, token2, 'operator after starting parenthesis')
    if (token1.paren === '[' && token2.type === 'operator' && !token2IsUnary)
      raiseParserError(string, token2, 'operator after starting bracket')
  }

  if (
    tokens[0].type === 'comma' ||
    (tokens[0].type === 'operator' &&
      !(tokens[0].op === '-' || tokens[0].op === '+'))
  )
    raiseParserError(string, { index: 0 }, 'expression begins with comma or operator')

  const lastToken = tokens[tokens.length - 1]
  if (lastToken.type === 'comma' || lastToken.type === 'operator')
    raiseParserError(string, lastToken, 'expression ends with comma or operator')
}

/**
 * Find a pair of parentheses in a list of tokens, namely the first one as indexed by the closing paren/bracket. For
 * example, in (x(y(z)(w))) it will find (z), returning [ paren1 index, paren2 index, paren1 token, paren2 token ]
 */
function findParenIndices (children: UnprocessedChild[]): [ number, number, Token, Token ] | null {
  let startIndex = -1
  let startToken: Token | null = null

  for (let i = 0; i < children.length; ++i) {
    let child = children[i]
    if (!('paren' in child)) continue

    if (child.paren === '(' || child.paren === '[') {
      startIndex = i
      startToken = child
    }

    if ((child.paren === ')' || child.paren === ']') && startIndex !== -1)
      return [ startIndex, i, startToken as Token, child ]
  }

  return null
}

/**
 * Convert constants and variables to their ASTNode counterparts
 */
function processConstantsAndVariables (tokens: (Token|ASTNode)[]) {
  for (let i = 0; i < tokens.length; ++i) {
    let token = tokens[i]
    let node

    if ('type' in token) {
      switch (token.type) {
        case 'constant':
          let type = toMathematicalType(isStringInteger(token.value) ? 'int' : 'real', true /* throw on error */)!
          node = new UnprocessedASTNode("constant", { value: token.value, token, startToken: token, endToken: token, type }, [])

          break
        case 'variable':
          node = new UnprocessedASTNode("variable", { name: token.name, token, startToken: token, endToken: token }, [])
          break
        default:
          continue
      }

      tokens[i] = node
    }
  }
}

// To process parentheses, we find pairs of them and combine them into ASTNodes containing the nodes and
// tokens between them. We already know the parentheses are balanced, which is a huge help here. We basically go
// through each node recursively and convert all paren pairs to a node, then recurse into those new nodes
function processParentheses (rootNode: UnprocessedASTNode) {
  rootNode.applyAll(node => {
    if (!(node instanceof UnprocessedASTNode)) return

    let parensRemaining = true

    while (parensRemaining) {
      parensRemaining = false
      let indices = findParenIndices(node.children)

      if (indices) {
        parensRemaining = true

        let [ startIndex, endIndex, startToken, endToken ] = indices

        let newNode = new UnprocessedASTNode("group",
          {}, [])

        let expr = node.children.splice(
          startIndex,
          endIndex - startIndex + 1,
          newNode
        )

        newNode.children = expr.slice(1, expr.length - 1)
      }
    }
  }, true)
}

// Turn function tokens followed by ASTNodes into OperatorNodes
function processFunctions (rootNode: UnprocessedASTNode) {
  rootNode.applyAll(node => {
    if (!(node instanceof UnprocessedASTNode)) return

    let children = node.children

    for (let i = 0; i < children.length; ++i) {
      let token = children[i]

      if (!(token instanceof UnprocessedASTNode) && token.type === 'function') {
        let nextNode = children[i + 1]
        if (!nextNode || !(nextNode instanceof UnprocessedASTNode)) {
          raiseUnknownParserError()
        }

        let newNode = new UnprocessedASTNode("operator",
          { name: token.name, isFunction: true }, nextNode.children)
        children[i] = newNode

        // Take children from the node coming immediately after
        newNode.children = nextNode.children
        // Remove the node immediately after
        children.splice(i + 1, 1)
      }
    }
  }, true)
}

// Given a node and an index i of a binary operator, combine the nodes immediately to the left and right of the node
// into a single binary operator
function combineBinaryOperator (node: UnprocessedASTNode, i: number) {
  const children = node.children
  let child = children[i]

  if (child instanceof UnprocessedASTNode || !('op' in child)) {
    raiseUnknownParserError()
  }

  let prevChild = children[i - 1]
  let nextChild = children[i + 1]

  if (!(prevChild instanceof UnprocessedASTNode) || !(nextChild instanceof UnprocessedASTNode)) {
    raiseUnknownParserError()
  }

  let newNode = new UnprocessedASTNode("operator",
    { name: child.op }, [ prevChild, nextChild ])

  children.splice(i - 1, 3, newNode)
}

// Process the highest precedence operators. Note that e^x^2 = (e^x)^2 and e^-x^2 = e^(-x^2).
function processUnaryAndExponentiation (root: UnprocessedASTNode) {
  root.applyAll(node => {
    if (!(node instanceof UnprocessedASTNode)) return

    let children = node.children

    // We iterate backwards
    for (let i = children.length - 1; i >= 0; --i) {
      let child = children[i]
      if (child instanceof UnprocessedASTNode || !('op' in child)) continue

      if (child.op === '-' || child.op === '+') {
        // If the preceding token is an unprocessed non-operator token, or node, then it's a binary expression
        let preceding = children[i-1]
        if (i !== 0 && ('type' in preceding) && (preceding.type !== 'operator')) continue

        let newNode = new UnprocessedASTNode("operator",
          { name: child.op }, [ children[i + 1] ])

        children.splice(i, 2, newNode)
      } else if (child.op === '^') {
        combineBinaryOperator(node, i)

        --i
      }
    }
  }, true)
}

// Combine binary operators, going from left to right, with equal precedence for all
function processOperators (root: UnprocessedASTNode, operators: string[]) {
  root.applyAll(node => {
    if (!(node instanceof UnprocessedASTNode)) return
    let children = node.children

    for (let i = 0; i < children.length; ++i) {
      let child = children[i]
      if (child instanceof UnprocessedASTNode || !('op' in child)) continue

      if (operators.includes(child.op)) {
        combineBinaryOperator(node, i)
        --i
      }
    }
  }, true)
}

// The index of each operator is also an enum, which is used in comparison chains to describe which operator is being used
const comparisonOperators = ['<', '<=', '==', '!=', '>=', '>']

// Process "comparison chains", which are sequences of the form 0 <= x < 2. Internally these are transformed into
// "comparison_chain" operators, which have the form comparison_chain(0, 1 (enum comparison), x, 0 (enum comparison), 2). Gross, but
// it's hard to cleanly represent these comparison chains otherwise. You *could* represent them using boolean operations,
// but that duplicates the internal nodes which is inefficient
function processComparisonChains (root: UnprocessedASTNode) {
  root.applyAll(node => {
    // TODO: process backwards for efficiency
    if (!(node instanceof UnprocessedASTNode)) return
    const children = node.children

    for (let i = 0; i < children.length; ++i) {
      let child = children[i]
      if (child instanceof UnprocessedASTNode || !('op' in child)) continue

      if (comparisonOperators.includes(child.op)) {
        let comparisonChainFound = false

        // Found a comparison operator token; we now check for whether the tokens +2, +4, etc. ahead of it are also
        // comparison tokens. If so, we emit a comparison chain

        // Index of the last comparison token, plus 2
        let j = i + 2
        for (; j < children.length; j += 2) {
          let nextChild = children[j]
          if (nextChild instanceof UnprocessedASTNode || !('op' in nextChild)) continue

          if (comparisonOperators.includes(nextChild.op)) {
            comparisonChainFound = true
          } else {
            break
          }
        }

        if (comparisonChainFound) {
          // The nodes i, i+2, i+4, ..., j-4, j-2 are all comparison nodes. Thus, all nodes in the range i-1 ... j-1
          // should be included in the comparison chain

          let comparisonChain = new UnprocessedASTNode("operator",
            { name: 'comparison_chain' }, [])

          // Looks something like [ ASTNode, '<', ASTNode, '<=', ASTNode ]
          let removedChildren = children.splice(
            i - 1,
            j - i + 1,
            comparisonChain // inserts the comparison chain as replacement
          )

          // [ ASTNode, ASTNode, ASTNode ]
          let cchainChildren: UnprocessedChildren = (comparisonChain.children = [])

          let comparisons: string[] = [] // [ '<', '<=' ]
          for (let i = 1; i < removedChildren.length - 2; i += 2) {
            let child = removedChildren[i]
            if (!('op' in child)) raiseUnknownParserError()

            comparisons.push(child.op)
          }

          for (let i = 0; i < removedChildren.length; i += 2) {
            cchainChildren.push(removedChildren[i])
          }

          comparisonChain.info.comparisons = comparisons

          return
        }
      }
    }
  }, true)
}

// Remove residual commas from the node
function removeCommas (root: UnprocessedASTNode) {
  root.applyAll(node => {
    if (!(node instanceof UnprocessedASTNode)) return
    let children = node.children
    let i = children.length

    while (i--) {
      if (children[i].type === 'comma') children.splice(i, 1)
    }
  }, true)
}

function verifyCommaSeparation (root: UnprocessedASTNode, string: string) {
  root.applyAll(node => {
    // Every function with multiple elements in it should have commas separating each argument, with no leading or
    // trailing commas.
    if (!(node instanceof UnprocessedASTNode)) return

    let children = node.children
    let isPlainGroup = node.nodeType === "group"

    let isFunction = !!node.info.isFunction
    if (!isFunction && !isPlainGroup) return

    if (children.length === 0) {
      // will eventually be fine. () is the empty tuple
      raiseParserError(string, { index: node.info.token?.index ?? -1 }, "empty parentheses")
      return
    }
    if (children[0].type === 'comma') raiseParserError(string, children[0], "leading comma in expression")

    // Must have the form "a,b,c"
    let prevChild: UnprocessedChild | null = null
    for (let i = 0; i < children.length; ++i) {
      let child = children[i]

      if ((!prevChild || prevChild.type === "comma") && child.type === "comma") {
        // child is a token
        raiseParserError(string, child, "spurious comma")
      }

      if (prevChild && prevChild.type !== "comma" && child.type !== "comma") {
        // child must be a node
        if (child instanceof UnprocessedASTNode) {
          raiseParserError(string, {index: child.info.token?.index ?? -1}, "trailing expression")
        }

        raiseUnknownParserError()
      }

      prevChild = child
    }

    // TODO tuples
    if (isPlainGroup) {
      if (children.length > 1) raiseParserError(string, { index: -1 }, "tuples not yet implemented")
    }

  }, true)
}

/**
 * Attach start and end tokens for each group
 * @param root
 */
function attachInformation (root: UnprocessedASTNode) {
  root.applyAll(node => {
    if (!(node instanceof UnprocessedASTNode)) return
    let info = node.info

    if (!info.token) {
      let firstChild = node.children[0]
      let lastChild = node.children[node.children.length - 1]

      if (!(firstChild instanceof UnprocessedASTNode) || !(lastChild instanceof UnprocessedASTNode)) {
        raiseUnknownParserError()
      }

      info.token = info.startToken = firstChild.info.token
      info.endToken = lastChild.info.token
    }
  }, true /* children first */)
}

/**
 * Parse a given list of tokens, returning a single ASTNode.
 * Perf: parseString("x^2+y^2+e^-x^2+pow(3,gamma(2401 + complex(2,3)))" took 0.028 ms / iteration as of Mar 14, 2022.
 * @param tokens
 * @param string String where tokens ultimately came from (used for descriptive error messages)
 */
function parseTokens (tokens: Token[], string: string): UnprocessedASTNode {
  // This is somewhat of a recursive descent parser because the grammar is nontrivial, but really isn't that
  // crazy. At intermediate steps, the node is a tree of both processed nodes and unprocessed tokens—a bit odd, but it
  // works.

  // Placed here because all further nodes will be groups or OperatorNodes
  processConstantsAndVariables(tokens)

  // Everything is done recursively within this root node
  let root = new UnprocessedASTNode("group", {}, tokens)

  processParentheses(root)
  processFunctions(root)

  // Order of operations: unary -/+, ^ (all right to left, together); PEMDAS
  processUnaryAndExponentiation(root)
  processOperators(root, ['*', '/'])
  processOperators(root, ['-', '+'])

  // Comparison chains are expressions of the form x <= y < z ..., which are combined into a single funky node called a
  // "comparison_chain" with arguments x, y, z and extra arguments <=, <
  processComparisonChains(root)
  processOperators(root, comparisonOperators)
  processOperators(root, ['and', 'or'])

  // Adds "debugging tokens", aka where each node starts and ends, and the top node
  attachInformation(root)
  verifyCommaSeparation(root, string)
  // processTuples(root, string)
  removeCommas(root)

  let c = root.children[0]
  if (!c) raiseUnknownParserError()

  if (c instanceof UnprocessedASTNode) {
    c.info.parsedFrom = string

    return c
  }

  raiseUnknownParserError()
}

function convertToASTNode(string: string, n: UnprocessedASTNode): ASTNode {
  let nn: ASTNode, isGroup = false

  switch (n.nodeType) {
    case "constant":
      nn = new ConstantNode(n.info as ConstantNodeParams)
      break
    case "group":
      nn = new ASTGroup(n.info as ASTGroupParams)
      isGroup = true
      break
    case "operator":
      nn = new OperatorNode(n.info as OperatorNodeParams)
      isGroup = true
      break
    case "variable":
      nn = new VariableNode(n.info as VariableNodeParams)
      break
    case "generic":
    default:
      raiseUnknownParserError()
  }

  if (isGroup) {
    let children: ASTNode[] = []

    for (let i = 0; i < n.children.length; ++i) {
      let child = n.children[i]

      if (!(child instanceof UnprocessedASTNode)) {
        raiseParserError(string, child /* token */, "unprocessed token")
      } else {
        children.push(convertToASTNode(string, child))
      }
    }

    // nn is an ast group by construction
    (nn as ASTGroup).children = children
  }

  return nn
}

function parseString (string: string): ASTNode {
  // noinspection ALL
  if (typeof string !== "string") {
    throw new ParserError("parseString expects a string")
  }

  checkParensBalanced(string)

  let tokens = getTokens(string)
  checkValid(tokens, string)

  let parsed: UnprocessedASTNode = parseTokens(tokens, string)

  return convertToASTNode(string, parsed)
}

export { parseString, getTokens }
