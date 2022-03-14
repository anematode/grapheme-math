import {toConcreteType} from "./builtin/builtin_types.js"

class EvaluationMode {
  constructor (name, params={}) {
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

  fillTypeMap (m) {
    for (let [ mathematical, concrete ] of Object.entries(m)) {
      if (typeof mathematical !== "string") throw new TypeError("unimplemented")

      this.typeMap.set(mathematical, toConcreteType(concrete))
    }
  }

  getConcreteType (mType) {
    return this.typeMap.get(mType.name) ?? null
  }
}

let normal = new EvaluationMode("normal", {
  typeMap: {
    "int": "int",
    "real": "real",
    "bool": "bool",
    "complex": "complex"
  }
})

let fastInterval = new EvaluationMode("fast_interval", {
  typeMap: {
    "int": "fast_int_interval",
    "real": "fast_real_interval",
    "bool": "fast_bool_interval"
  }
})

export const EvaluationModes = new Map()
EvaluationModes.set("normal", normal)
EvaluationModes.set("fast_interval", fastInterval)

export function toEvaluationMode(o) {
  if (o instanceof EvaluationMode) return o
  let mode = EvaluationModes.get(o)

  if (!mode) throw new Error("Unrecognized evaluation mode " + o)
  return mode
}
