import {resolveOperatorDefinition} from './builtin/builtin_operators.js'
import {toMathematicalType} from "./builtin/builtin_types.js"
import {EvaluationMode, toEvaluationMode} from "./eval_modes.js"
import {MathematicalConstants} from "./builtin/globals.js"
import {localWarn} from "../utils.js";
import {MathematicalType} from "./type.js";
import {MathematicalCast, OperatorDefinition} from "./operator_definition.js";
import { KeywordOperatorName } from "./parse";

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

export class EvaluationError extends Error {
  constructor (message) {
    super(message)

    this.name = 'EvaluationError'
  }
}

export class ResolutionError extends Error {
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

export type ResolveTypesOptions = {
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

export type VariableDependency = {
  type: MathematicalType
  operatorDefinition: null | OperatorDefinition
  count: number
}

export type VariableDependencies = Map<string, VariableDependency>

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

export class ASTNode {
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
     * @type {{}}
     */
    this.info = params.info ?? {}

    /**
     * The node's operator. If a constant, this will be null and the value will be converted later. If an operator, this
     * must not be null (or the definition is not known). If a variable, this will be called if this is not null.
     * @type {null|OperatorDefinition}
     */
    this.operatorDefinition = params.operatorDefinition ?? null

    /**
     * Highest node in this tree
     * @type {null|ASTNode}
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

export type ASTGroupParams = ASTNodeParams & {
  children?: ASTNode[]
}

// Node with children. A plain ASTGroup is usually just a parenthesized thing
export class ASTGroup extends ASTNode {
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

export type ConstantNodeParams = ASTNodeParams & {
  value: string
  type: MathematicalType | null // type required for constants
}

export class ConstantNode extends ASTNode {
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

export type VariableNodeParams = ASTNodeParams & {
  name: string
  operatorDefinition?: OperatorDefinition | null
}

export class VariableNode extends ASTNode {
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

export type OperatorNodeParams = ASTGroupParams & {
  name: string
  extraArgs?: { [key: string]: string },
  casts: MathematicalCast[] | null
}

export class OperatorNode extends ASTGroup {
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
