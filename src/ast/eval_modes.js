

// List of evaluation modes.

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

  fillTypeMap () {
    
  }
}

let normal = new EvaluationMode("normal", {
  typeMap: {
    "int": "int",
    "real": "real",
    "complex": "complex"
  }
})

let fastInterval = new EvaluationMode("fast_interval", {

})

let EvaluationModes = new Map()
EvaluationModes.set("normal", normal)
EvaluationModes.set("fast_interval", fastInterval)

export function toEvaluationMode(o) {
  if (o instanceof EvaluationMode) return o
  let mode = EvaluationModes.get(o)

  if (!mode) throw new Error("Unrecognized evaluation mode " + o)
}
