import { resolveOperatorDefinition } from './builtin_operators.js'
import { toMathematicalType } from "./builtin_types.js"
import { toEvaluationMode } from "./eval_modes.js"

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


/**
 * Helper function (doesn't need to be fast)
 * @param node {ASTNode}
 * @param name {string}
 * @param keys {Array<string>} Keys to look for
 * @param params {{}}
 * @returns {string}
 */
function prettyPrintNode(node, name, keys, params) {
  let out = []

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

export class ASTNode {
  constructor (params={}) {
    /**
     * MathematicalType of the node (int, complex, etc.). Null if not resolved
     */
    this.type = params.type ?? null

    /**
     * Other info about the node (for example, where it was in a parsed string)
     * @type {{}}
     */
    this.info = params.info ?? {}
  }

  applyAll (func, onlyGroups=false, childrenFirst=false, depth=0) {
    if (!onlyGroups) func(this, depth)
  }

  isGroup () {
    return false
  }

  /**
   * Is this node an operator node that is a function (and not an infix, prefix or postfix operator)
   * @returns {boolean}
   */
  isFunctionNode () {
    return false
  }

  toString () {
    return `[object ${this.getNodeTypeAsString()}]`
  }

  getNodeType () {
    return 0
  }

  /**
   * Get the node type as a string instead of an enum
   * @returns {string}
   */
  getNodeTypeAsString () {
    switch (this.getNodeType()) {
      case 0: return "ASTNode"
      case 1: return "ConstantNode"
      case 2: return "VariableNode"
      case 3: return "OperatorNode"
      case 4: return "ASTGroup"
    }

    return "UnknownNode"
  }

  /**
   * For debug use only. Example: OperatorNode{type=int, name="+", children=List{ConstantNode{type=int, value="3"}, VariableNode{type=int, name="x"}}}
   */
  prettyPrint (params={}) {
    return prettyPrintNode(this, this.getNodeTypeAsString(), KNOWN_KEYS, params)
  }

  // Enum of node types
  static TYPES = Object.freeze({
    ASTNode: 0,
    ConstantNode: 1,
    VariableNode: 2,
    OperatorNode: 3
  })

  clone () {
    return new ASTNode(this)
  }

  /**
   * Figure out the type of each node, given the type of each variable node within it.
   * Perf: on "x^2+y^2+e^-x^2+pow(3,gamma(2401 + complex(2,3)))", took 0.002 ms / iteration as of Mar 14, 2022
   * @param opts
   */
  resolveTypes (opts) {
    // Convert all arg values to mathematical types

    opts ??= {}
    this.applyAll(node => node._resolveTypes(opts), false /* only groups */, true /* children first */)
  }

  _resolveTypes (args) {

  }

  /**
   * Evaluate the function in place (without compilation), passing variable values as a dictionary. This is SLOW. Don't
   * call it unless you're only going to evaluate the function a handful of times
   * @param vars
   * @param opts
   */
  evaluate (vars, opts={}) {
    let mode = toEvaluationMode(opts.mode ?? "normal") // throws on fail

    return this._evaluate(vars, mode, opts)
  }

  _evaluate (vars, mode, opts) {
    throw new Error("ASTNode cannot be evaluated")
  }
}

// Node with children. A plain ASTGroup is usually just a parenthesized thing
export class ASTGroup extends ASTNode {
  constructor (params={}) {
    super(params)

    this.info.isFunction = false
  }

  /**
   * Apply a function to this node and all of its children, recursively.
   * @param func {Function} The callback function. We call it each time with (node, depth) as arguments
   * @param onlyGroups {boolean} Only call the callback on groups
   * @param childrenFirst {boolean} Whether to call the callback function for each child first, or for the parent first.
   * @param depth {number}
   * @returns {ASTNode}
   */
  applyAll (func, onlyGroups = false, childrenFirst = false, depth = 0) {
    if (!childrenFirst) func(this, depth)

    let children = this.children
    for (let i = 0; i < children.length; ++i) {
      let child = children[i]
      if (child instanceof ASTNode && (!onlyGroups || child.isGroup())) {
        child.applyAll(func, onlyGroups, childrenFirst, depth + 1)
      }
    }

    if (childrenFirst) func(this, depth)

    return this
  }

  isGroup () {
    return true
  }

  getNodeType () {
    return 4
  }

  clone () {
    return new ASTGroup(this)
  }

  _resolveTypes (args) {
    // Only called on a raw group, aka a parenthesized group
    let children = this.children

    for (let i = 0; i < children.length; ++i) {
      children[i]._resolveTypes(args)
    }

    this.type = children[0].type
  }

  _evaluate (vars, mode, opts={}) {
    return this.children[0]._evaluate(vars, mode, opts)
  }
}

export class ConstantNode extends ASTNode {
  constructor (params={}) {
    super(params)

    // Generally a text rendition of the constant node; e.g., "0.3" or "50"
    this.value = params.value
  }

  getNodeType () {
    return 1
  }

  clone () {
    return new ConstantNode(this)
  }

  _resolveTypes (args) {

  }

  _evaluate (vars, mode, opts={}) {
    let type = mode.getConcreteType(this.type)

    if (!type){
      throw new EvaluationError(`Cannot find concrete type in mode ${mode.name} for mathematical type ${this.type.toHashStr()}`)
    }

    return type.castPermissive(this.value) // basically never throws
  }
}

export class VariableNode extends ASTNode {
  constructor (params={}) {
    super(params)

    this.name = params.name
    if (!this.name || typeof this.name !== "string")
      throw new Error("Variable name must be a string")
  }

  getNodeType () {
    return 2
  }

  clone() {
    return new VariableNode(this)
  }

  _resolveTypes (args) {
    let { vars, defaultType } = args

    let info
    if (vars)
      info = vars[this.name]

    this.type = toMathematicalType(info ?? (defaultType ?? "real"))
  }

  _evaluate (vars, mode, opts={}) {
    let v = vars[this.name]
    if (v === undefined) {
      throw new EvaluationError(`Variable ${this.name} is not defined in the current scope`)
    }

    return v
  }
}

export class OperatorNode extends ASTGroup {
  constructor (params={}) {
    super(params)

    this.name = params.name
    if (!this.name || typeof this.name !== "string")
      throw new Error("Operator name must be a string")

    // Arguments to the operator
    this.children = params.children ?? []

    // Extra arguments that have an effect on the operator's mathematical meaning, but which are unwieldy to represent
    // directly as an argment. Current use: comparison chain, where the arguments are the comparisons to be done and
    // extraArgs.comparisons is, say, [ '<', '<=' ]
    this.extraArgs = params.extraArgs ?? {}

    /**
     * Which operator is actually being used here
     * @type {null|OperatorDefinition}
     */
    this.operatorDefinition = null

    /**
     * Array of casts needed
     * @type {null|MathematicalCast[]}
     */
    this.casts = null
  }

  getNodeType () {
    return 3
  }

  isFunctionNode() {
    return !!this.info.isFunction
  }

  clone () {
    return new OperatorNode(this)
  }

  childArgTypes () {
    return this.children.map(c => c.type)
  }

  _resolveTypes (args) {
    let childArgTypes = this.children.map(c => c.type)

    fail: {
      for (let t of childArgTypes) {
        if (t == null) {
          break fail
        }
      }

      let [definition, casts] = resolveOperatorDefinition(this.name, childArgTypes)

      if (definition == null || !casts.every(cast => cast !== null)) {
        break fail
      }

      this.type = definition.returns
      this.operatorDefinition = definition
      this.casts = casts
      return
    }

    this.type = null
    this.operatorDefinition = null
    this.casts = null
  }

  _evaluate (vars, mode, opts={}) {
    if (!this.operatorDefinition) throw new EvaluationError("Operator definition not resolved")
    if (!this.casts) throw new EvaluationError("Casts not resolved")

    let casts = this.casts
    let childrenValues = this.children.map((c, i) => {
      let cast = casts[i]
      let ccast = cast.getDefaultEvaluator(mode)
      if (ccast === null) {
        throw new EvaluationError(
          `No concrete cast (in mode ${mode.name}) between source ${mode.getConcreteType(cast.srcType())}`
            + `and destination ${mode.getConcreteType(cast.dstType())}`)
      }

      let castedValue = ccast.callNew([
        c._evaluate(vars, mode, opts) // compute child
      ])

      return castedValue
    })

    let evaluator = this.operatorDefinition.getDefaultEvaluator(mode)
    return evaluator.callNew(childrenValues)
  }
}
