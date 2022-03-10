import { toMathematicalType } from './builtin_types.js'
import { MathematicalType } from './type.js'
import { castDistance } from './evaluator.js'

/**
 * Attempt conversion from array of types to corresponding mathematical types
 * @param args {any}
 * @returns MathematicalType[]
 */
function convertArgumentTypes(args) {
  if (args == null) return []
  if (!Array.isArray(args)) throw new TypeError("Expected argument type list to be an array")

  let converted = args.map(toMathematicalType)

  // Validate arguments
  for (let i = 0; i < args.length; ++i) {
    let arg = converted[i]
    if (!arg) {
      throw new Error(`Unknown argument type at index ${i} (attempted conversion from ${args[i]})`)
    }
  }

  return converted
}

export class OperatorDefinition {
  constructor (params={}) {
    /**
     * Readable name of the operator that identifies it: e.g., "^", "/", "gamma"
     * @type {string}
     */
    this.name = params.name

    /**
     * Arguments
     * @type {MathematicalType[]}
     */
    this.args = convertArgumentTypes(params.args)

    /**
     * Return type (void type if nothing)
     * @type {MathematicalType}
     */
    this.returns = toMathematicalType(params.returns ?? "void")
    if (!this.returns) {
      throw new Error(`Unknown return type (attempted conversion from ${params.returns})`)
    }

    /**
     * List of concrete evaluators that may be searched through
     */
    this.evaluators = params.evaluators ?? []

    this.tags = { builtin: !!params.builtin }
    if (params.tags) Object.assign(this.tags, params.tags)
  }

  argCount () {
    return this.args.length
  }

  /**
   * Check whether this operator can be called with the given mathematical types.
   * @param args {MathematicalType[]}
   * @returns {number} -1 if it cannot be called, a nonnegative integer giving the number of necessary implicit casts to call it
   */
  canCallWith (args) {
    return castDistance(this.getCasts(args))
  }

  getCasts (args) {
    if (this.args.length !== args.length) return null

    let casts = []
    for (let i = 0; i < args.length; ++i) {
      let cast = getMathematicalCast(args[i] /* src */, this.args[i])

      if (!cast) return null
      casts.push(cast)
    }

    return casts
  }

  prettyPrint() {
    // ^(int, int) -> int
    return `${this.name}(${this.args.map(arg => arg.prettyPrint()).join(', ')}) -> ${this.returns.prettyPrint()}`
  }

  getEvaluator (args, returns, { evaluatorType = "returns" } = {}) {
    if (args.length !== this.args.length) return null

    ev: for (const e of this.evaluators) {
      for (let i = 0; i < this.args.length; ++i) {
        if (args[i].toHashStr() !== this.args[i].toHashStr()) {
          continue ev
        }
      }

      if (e.returns !== returns) continue
      if (e.type !== evaluatorType) continue

      return e
    }

    return null
  }
}

// A cast is just a special operator that converts one type to another
export class MathematicalCast extends OperatorDefinition {
  constructor (params) {
    if (!params.src || !params.dst) throw new Error("No source or destination types provided")

    params.args = [ params.src ]
    params.returns = params.dst

    super(params)

    this.name = this.name ?? this.returns.toHashStr()
  }

  srcType () {
    return this.args[0]
  }

  dstType () {
    return this.returns
  }
}

// Maps from src hash to map from dst hash to cast
const BUILTIN_MATHEMATICAL_CASTS = new Map()

/**
 * Register a mathematical cast from src to dst
 * @param cast
 */
export function registerMathematicalCast (cast) {
  const CASTS = BUILTIN_MATHEMATICAL_CASTS

  let srcType = cast.srcType().toHashStr()
  let dstType = cast.dstType().toHashStr()

  if (!CASTS.has(srcType))
    CASTS.set(srcType, new Map())
  let srcCasts = CASTS.get(srcType)

  srcCasts.set(dstType, cast)
  return cast
}

/**
 * Get cast from src to dst. Returns "null" if the cast doesn't exist, "identity" if the types are the same, and a
 * corresponding MathematicalCast if there is a match
 * @param srcType
 * @param dstType
 */
export function getMathematicalCast (srcType, dstType) {
  if (!(srcType instanceof MathematicalType) || !(dstType instanceof MathematicalType))
    throw new Error("Invalid source or destination type")

  if (srcType.isSameType(dstType)) return "identity"

  let srcCasts = BUILTIN_MATHEMATICAL_CASTS.get(srcType.toHashStr())
  if (!srcCasts) return null

  return srcCasts.get(dstType.toHashStr()) ?? null
}

export function canMathematicalCast (srcType, dstType) {
  return !!getMathematicalCast(srcType, dstType)
}
