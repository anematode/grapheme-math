import { toMathematicalType } from './builtin/builtin_types.js'
import { ConcreteType, MathematicalType } from './type.js'
import {castDistance, ConcreteCast, ConcreteEvaluator} from './evaluator.js'
import {EvaluationMode, EvaluationModes} from "./eval_modes.js"

/**
 * Attempt conversion from array of types to corresponding mathematical types
 * @param args {any}
 * @returns MathematicalType[]
 */
function convertArgumentTypes(args: any): Array<MathematicalType> {
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

type CastOperatorDefinitionBaseParams = {
  evaluators: Array<ConcreteEvaluator>

  tags?: OperatorDefinitionTags
  builtin?: boolean
  constant?: boolean
}

type OperatorDefinitionParams = CastOperatorDefinitionBaseParams & {
  name: string
  args: Array<MathematicalType|string>
  returns: MathematicalType|string
}

type CastDefinitionParams = CastOperatorDefinitionBaseParams & {
  src: MathematicalType | string
  dst: MathematicalType | string
}

type OperatorDefinitionTags = {
  builtin: boolean
  constant: boolean
}

type EvaluatorPreferences = {
  evalType: "new" | "writes"
}

export class OperatorDefinition {
  name: string
  args: Array<MathematicalType>
  returns: MathematicalType
  evaluators: Array<ConcreteEvaluator>
  tags: OperatorDefinitionTags

  /**
   * Evaluators within this.evaluators that will be used immediately in a given evaluation mode, without any other
   * conditions in place. In particular, these are evaluators with all the correct matching types, and preferring a
   * "new" rather than a "writes" evaluator. Note that any "writes" evaluator can be used as a "new" evaluator with
   * the callNew(args) function.
   */
  defaultEvaluators: Map<string, ConcreteEvaluator>

  constructor (params: OperatorDefinitionParams) {
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

    this.tags = {
      builtin: !!params.builtin, // includes generated definitions, but not user-generated defs
      constant: !!params.constant // whether function is totally constant (e.g., x => 1)
    }

    if (params.tags) Object.assign(this.tags, params.tags)

    this.defaultEvaluators = new Map()
    this.fillDefaultEvaluators()
  }

  /**
   * Compute evaluators for each mode, a (not necessarily strict) subset of all available evaluators. These define the
   * "most canonical" evaluation of the expression; the default evaluator of a given mode is the one that should be
   * called in an evaluate() invocation, for example.
   */
  fillDefaultEvaluators () {
    let evaluators = this.evaluators, args = this.args
    let possibleSignatures = Array.from(EvaluationModes.values()).map(mode => ({ // for each mode, get the signature
      mode,
      returns: mode.getConcreteType(this.returns),
      args: args.map(mt => mode.getConcreteType(mt))
    })).filter(({ returns, args }) => returns !== null && args.every(a => a !== null)) // eliminate signatures w/ missing types

    for (let p of possibleSignatures) {
      let { mode, returns, args } = p
      let foundEvaluator

      for (let e of evaluators) {
        // See if the evaluator matches the signature. null returns and args are filtered out above
        if (e.returns.isSameConcreteType(returns!) &&
          args.every((arg, i) => arg!.isSameConcreteType(e.args[i]))) {

          foundEvaluator = e
          if (e.evalType === "new") { // prefer "new" evaluators
            break
          }
        }
      }

      if (foundEvaluator) {
        this.defaultEvaluators.set(mode.name, foundEvaluator)
      }
    }
  }

  /**
   * Get the default evaluator for a given mode (null if doesn't exist)
   * @param mode {EvaluationMode}
   * @returns {ConcreteEvaluator|null}
   */
  getDefaultEvaluator (mode: EvaluationMode): ConcreteEvaluator | null {
    return this.defaultEvaluators.get(mode.name) ?? null
  }

  findEvaluator (args: Array<ConcreteType>, preferences: EvaluatorPreferences): ConcreteEvaluator | null {
    let evaluators = this.evaluators

    let { evalType } = preferences
    let best: ConcreteEvaluator | null = null

    for (let i = 0; i < evaluators.length; ++i) {
      let e = evaluators[i]
      let dist = e.castDistance(args)

      if (dist === 0) {
        best = e

        if (e.evalType === evalType) {
          break
        }
      }
    }

    return best
  }

  /**
   * Check whether this operator can be called with the given mathematical types.
   * @param args
   * @returns -1 if it cannot be called, a nonnegative integer giving the number of necessary implicit casts to call it
   */
  canCallWith (args: Array<MathematicalType>): number {
    let casts = this.getCasts(args)

    if (!casts) return -1
    return castDistance(casts)
  }

  /**
   * Get a list of mathematical casts from source types to the required types for this operator.
   * @param args
   */
  getCasts (args: Array<MathematicalType>): Array<MathematicalCast> | null {
    if (this.args.length !== args.length) return null

    let casts: Array<MathematicalCast> = []
    for (let i = 0; i < args.length; ++i) {
      let cast = getMathematicalCast(args[i] /* src */, this.args[i])

      if (!cast) return null
      casts.push(cast)
    }

    return casts
  }

  prettyPrint(): string {
    // ^(int, int) -> int
    return `${this.name}(${this.args.map(arg => arg.prettyPrint()).join(', ')}) -> ${this.returns.prettyPrint()}`
  }
}



/**
 * A mathematical cast is just a special operator that converts one type to another, accepting a single argument and
 * returning the destination type
 */
export class MathematicalCast extends OperatorDefinition {
  constructor (params: CastDefinitionParams) {
    if (!params.src || !params.dst) throw new Error("No source or destination types provided")
    let nParams: OperatorDefinitionParams = { ...params, name: params.src.toString(), args: [ params.src ], returns: params.dst }

    super(nParams)

    this.name = this.name ?? this.returns.toHashStr()
  }

  /**
   * Source type
   */
  srcType (): MathematicalType {
    return this.args[0]
  }

  /**
   * Destination type
   * @returns {MathematicalType}
   */
  dstType () {
    return this.returns
  }

  /**
   * Whether this cast IS the identity cast. This function distinguishes the single, canonical identity cast
   * (which should only be used between objects of identical type) and everything else, including "mathematically
   * identical" casts like int -> real.
   * @returns {boolean}
   */
  isIdentity() {
    return false
  }
}

export class IdentityMathematicalCast extends MathematicalCast {
  isIdentity() {
    return true
  }
}

/**
 * Identity casts generated on a per-type basis (somewhat of a formalism; in a compiled setting these will all be elided)
 * @type {Map<string, IdentityMathematicalCast>}
 */
const CachedIdentityCasts = new Map()

/**
 * Generate formal identity evaluators for a given mathematical cast
 * @param srcType
 * @returns
 */
function generateIdentityEvaluators(srcType: MathematicalType): Array<ConcreteCast> {
  let evaluators: Array<ConcreteCast> = []

  for (let mode of EvaluationModes.values()) {
    let concreteType = mode.getConcreteType(srcType)

    if (concreteType != null) // only get casts for which there are corresponding concrete types
      evaluators.push(new ConcreteCast({
        identity: true,
        src: concreteType,
        dst: concreteType,
        func: x => x   /* TODO: examine logical consistency here */
      }))
  }

  return evaluators
}

/**
 * Generate a cached IdentityCast object for the given source type
 * @param srcType
 */
function generateIdentityCast (srcType: MathematicalType): IdentityMathematicalCast {
  let s = srcType.toHashStr()
  let c = CachedIdentityCasts.get(s)

  if (!c) {
    CachedIdentityCasts.set(s, c = new IdentityMathematicalCast({
      src: srcType,
      dst: srcType,
      evaluators: generateIdentityEvaluators(srcType)
    }))
  }

  return c
}

/**
 * First map key is source type; second map key is destination type
 */
const BuiltinMathematicalCasts: Map<string, Map<string, MathematicalCast>> = new Map()

/**
 * Register a mathematical cast from src to dst
 * @param cast
 */
export function registerMathematicalCast (cast) {
  const CASTS = BuiltinMathematicalCasts

  let srcType = cast.srcType().toHashStr()
  let dstType = cast.dstType().toHashStr()

  if (!CASTS.has(srcType))
    CASTS.set(srcType, new Map())
  let srcCasts = CASTS.get(srcType)

  srcCasts!.set(dstType, cast)
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

  if (srcType.isSameType(dstType))
    return generateIdentityCast(srcType)

  let srcCasts = BuiltinMathematicalCasts.get(srcType.toHashStr())
  if (!srcCasts) return null

  return srcCasts.get(dstType.toHashStr()) ?? null
}
