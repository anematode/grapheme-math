
// Evaluators are functions of a specific signature that compute some operator. They are less abstract than the
// operators themselves; for example, * may have multiple evaluators: (int, int) -> int, (complex, complex) -> complex.
// In turn, evaluators are abstractions over concrete evaluators, which have a signature of concrete types and actual
// functions to be evaluated. Concrete evaluators do the actual legwork! In the case of primitive returns, they return
// the result directly; in the case of non-primitive returns, they will usually have two forms: one form that simply
// takes the operator arguments and returns the result, and one that takes in an extra parameter, the *destination*, and
// writes the result to the destination. This avoids unnecessary allocations and is especially useful in the fast
// interval case.

// Evaluators of all types have metadata that allow for proper usage and various optimizations. Some important ones
// common to both types:
// identity: true/false; whether the evaluator is an identity operation, either abstractly or concretely. For example,
// the cast from concrete int -> real is an identity. Almost all identities in practice are concrete casts, like
// concrete int -> real, since both use numbers
//
// Data particular to concrete evaluators:
// func: A function that can actually be invoked (with the appropriate arguments)
// primitive: If an empty string, func *must* be used. If non-empty, it may be used as a literal string in code.
// For example, concrete +(int, int) -> int has primitive="+", so that func doesn't have to be used at all in the
// compiled code. If it has no arguments, then the string itself can be used (for example, a hypothetical zero() -> int
// might have primitive="0"). If it has one argument, then the argument is a unary prefix. If it has two arguments, then
// the argument is a binary prefix.
// type: "new" or "write". If "new", takes in arguments and returns the result. If "write", takes in arguments and
// writes the result to the last argument.

// Casts are a special form of evaluator that take in a single argument and return another argument. As with most
// things, there are both abstract and concrete casts. For example, real -> complex is an implicit abstract
// cast and a concrete cast. What's special about casts, though, is that there are casts between concrete types that
// usually are used in different modes. For example, real -> fast_interval_real.

import { toConcreteType } from './builtin/types.js'
import {ConcreteType} from "./type.js"
import { MathematicalCast } from "./operator_definition.js";

const jsPrimitives = Object.freeze(['+', '-', '/', '*', '&&', '||', '==', '!=', '<=', '>=', '<', '>'] as const)
const unaryPrimitives = {
  '-': x => -x
}

// @ts-ignore
const binaryPrimitives: { [key in typeof jsPrimitives[number]]: Function } = {}
// Fill binary primitives
;jsPrimitives.forEach(op => {
  binaryPrimitives[op] = (new Function('x', 'y', `return x ${op} y`))
})

export type AllowedJSPrimitive = (typeof jsPrimitives)[number] | ""

type ConcreteEvaluatorParams = {
  args: (string|ConcreteType)[]
  returns: string|ConcreteType
  func?: Function // required if primitive is not specified

  /** @defaultValue new */
  evalType?: "new" | "write"

  /** @defaultValue false */
  identity?: boolean

  /** @defaultValue "" */
  primitive?: AllowedJSPrimitive

  /** @defaultValue false */
  isConstant?: boolean
}

export class ConcreteEvaluator {
  // Argument types
  args: ConcreteType[]
  // Return type
  returns: ConcreteType
  // Number of arguments (calculated automatically)
  argCount: number
  // Whether the evaluator accepts a single argument and returns that argument immediately
  identity: boolean
  // If not the empty string, the equivalent JS primitive that may be used
  primitive: AllowedJSPrimitive
  /**
   * Either "new" or "write". "new" means the func returns a new instance of the object. "write" means the function
   * writes the result to the last argument (arg1, arg2, dst). For example, a "write" +(complex, complex, complex)
   * would put the result of the addition of the first two numbers into the second, overwriting whatever was there
   */
  evalType: "new" | "write"
  // Underlying JS function that can be called
  func: Function
  // Completely constant evaluator, e.g., () => Math.E
  isConstant: boolean

  constructor (params: ConcreteEvaluatorParams) {
    // @ts-ignore (check occurs immediately)
    this.args = (params.args ?? []).map(toConcreteType)
    if (!this.args.every(arg => !!arg)) throw new Error("Unknown argument type")

    let returns = toConcreteType(params.returns ?? "void")
    if (!returns) throw new Error("Unknown return type")

    this.returns = returns

    this.argCount = this.args.length
    this.identity = !!params.identity

    this.evalType = params.evalType ?? "new"
    if (this.evalType !== "new" && this.evalType !== "write") {
      throw new Error("Evaluator type must be either new or write, not " + this.evalType)
    }

    // Primitive evaluator symbol (that can basically be evaled, for example "+" in +(real, real) -> real)
    this.primitive = params.primitive ?? ""
    this.func = params.func ?? this.getDefaultFunc()
    this.isConstant = !!params.isConstant

    this.applyTag()
  }

  getDefaultFunc(): Function {
    if (this.returns.isPrimitive && this.evalType === "write") throw new Error("Cannot write to a primitive")

    let func: Function | null = null

    if (this.identity) {
      if (this.evalType === "new") {
        func = this.returns.clone
      } else if (this.evalType === "write") {
        func = this.returns.copyTo
      }
    } else if (this.primitive) {
      if (this.argCount === 0) {
        func = new Function("return " + this.primitive)
      } else if (this.argCount === 1) {
        func = unaryPrimitives[this.primitive]
      } else if (this.argCount === 2) {
        func = binaryPrimitives[this.primitive]
      }

      this.evalType = "new"
    }

    if (!func) throw new Error("Unable to generate evaluation function")

    return func
  }

  applyTag () {

  }

  /**
   * Given a list of concrete types, whether the evaluator can be called with those types. -1 if not, 0 if no casts are
   * needed, >0 for the number of needed casts
   * @param args
   */
  castDistance (args: ConcreteType[]): number {
    let casts = this.getCasts(args)
    if (!casts) return -1

    return castDistance(casts)
  }

  getCasts (args: ConcreteType[]): ConcreteCast[] | null {
    if (this.args.length !== args.length) return null

    let casts: ConcreteCast[] = []
    for (let i = 0; i < args.length; ++i) {
      let cast = getConcreteCast(args[i] /* src */, this.args[i])

      if (!cast) return null
      casts.push(cast)
    }

    return casts
  }

  /**
   * Call an evaluator as if it were new (creating a new value if it's a writes evaluator)
   * @param args
   */
  callNew (args: any[]): any {
    if (this.evalType === "new") {
      return this.func(...args)
    }

    let writeTo = this.returns.init()
    this.func(...args, writeTo)

    return writeTo
  }
}

// A concrete cast is a special evaluator taking in one concrete type and outputting another. Note the concrete types
// might be of the same mathematical type! For example, the concrete cast real -> real_interval makes quite a bit of
// sense; 3.2 -> [3.2, 3.2].
export class ConcreteCast extends ConcreteEvaluator {
  constructor (params) {
    if (!params.src || !params.dst) throw new Error("No source or destination types provided")

    params.args = [ params.src ]
    params.returns = params.dst

    super(params)
  }

  srcType (): ConcreteType {
    return this.args[0]
  }

  dstType (): ConcreteType {
    return this.returns
  }

  /**
   * Whether this cast is a logical identity cast (still differentiating between types that have the same
   * underlying type, like real and int)
   * @returns {boolean}
   */
  isIdentity (): boolean {
    return false
  }
}

// TODO logical concrete casts for each concrete type
export class IdentityConcreteCast extends ConcreteCast {
  isIdentity () {
    return true
  }

  callNew (args) {
    return args[0]
  }
}

let I = new IdentityConcreteCast({
  // TODO
  src: "int",
  dst: "int",
  func: x => x
})

// src? -> dst? -> cast?
const BUILTIN_CONCRETE_CASTS: Map<string, Map<string, ConcreteCast>> = new Map()

/**
 * Register a concrete cast from src to dst
 * @param cast
 */
export function registerConcreteCast (cast: ConcreteCast): ConcreteCast {
  const CASTS = BUILTIN_CONCRETE_CASTS

  let srcType = cast.srcType().toHashStr()
  let dstType = cast.dstType().toHashStr()

  if (!CASTS.has(srcType))
    CASTS.set(srcType, new Map())
  let srcCasts = CASTS.get(srcType)! // set above

  srcCasts.set(dstType, cast)

  return cast
}

/**
 * Get cast from src to dst. Returns "null" if the cast doesn't exist, "identity" if the types are the same, and a
 * corresponding ConcreteCast if there is a match
 * @param srcType
 * @param dstType
 */
export function getConcreteCast (srcType: ConcreteType, dstType: ConcreteType): ConcreteCast | null {
  if (srcType.isSameConcreteType(dstType)) return I

  let srcCasts = BUILTIN_CONCRETE_CASTS.get(srcType.toHashStr())
  if (!srcCasts) return null

  return srcCasts.get(dstType.toHashStr()) ?? null
}

export function canConcreteCast (srcType: ConcreteType, dstType: ConcreteType): boolean {
  return !!getConcreteCast(srcType, dstType)
}

export function getConcreteCasts (): ConcreteCast[] {
  let casts: ConcreteCast[] = []
  for (let castList of BUILTIN_CONCRETE_CASTS.values()) {
    casts.push(...castList.values())
  }
  return casts
}

/**
 * Determine the number of non-identity casts in a list of casts, and -1 if there is an empty cast somewhere
 * @param casts
 * @returns {number}
 */
export function castDistance (casts: ConcreteCast[] | MathematicalCast[]): number {
  let count = 0

  for (let cast of casts) {
    if (!cast.isIdentity())
      count++
    else if (!cast) {
      count = -1
      break
    }
  }

  return count
}
