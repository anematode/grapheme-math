import {toConcreteType} from "./builtin/builtin_types.js"
import {ConcreteType, MathematicalType} from "./type";

type AcceptableTypeMap = Map<string, ConcreteType> | { [key: string]: string }
type EvaluationModeParams = {
  args?: Array<any>  // default is []
  typeMap: AcceptableTypeMap
}

export class EvaluationMode {
  name: string
  args: Array<any>
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
      if (typeof mathematical !== "string") throw new TypeError("unimplemented")

      this.typeMap.set(mathematical, toConcreteType(concrete))
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
    "int": "fast_int_interval",
    "real": "fast_real_interval",
    "bool": "fast_bool_interval"
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
      throw new Error("Unrecognized evaluation mode " + o)
    } else {
      throw new Error("Evaluation mode must be a string ('normal') or EvaluationMode object")
    }
  }

  return mode
}
