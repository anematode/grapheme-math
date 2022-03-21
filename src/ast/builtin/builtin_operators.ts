import { OperatorDefinition } from "../operator_definition.js"
import { castDistance, ConcreteEvaluator } from '../evaluator.js'
import {gammaReal} from "../../real/normal.js"
import {Complex} from "../../complex/normal.js"

// For now we'll just have a mapping  name -> Array of possibilities
const KNOWN_OPERATORS = new Map()

// Put an operator in the global list
function registerOperator(definition) {
  let name = definition.name

  if (!KNOWN_OPERATORS.has(name))
    KNOWN_OPERATORS.set(name, [])

  KNOWN_OPERATORS.get(name).push(definition)
}

/**
 * Get the corresponding OperatorDefinition of a given name and arg types, returning [ definition, casts ] on success,
 * where casts is an array of MathematicalCasts, and [ null, null ] on failure
 * @param name
 * @param argTypes
 */
export function resolveOperatorDefinition (name, argTypes) {
  let defs = KNOWN_OPERATORS.get(name)
  if (!defs) return [ null, null ]

  // Choose first definition with the least cast distance (may change later)
  let bestDef = null
  let bestCasts = null
  let bestDist = Infinity

  for (let def of defs) {
    let casts = def.getCasts(argTypes)

    if (casts) {
      let dist = castDistance(casts)
      if (dist < bestDist) {
        bestDef = def
        bestCasts = casts
        bestDist = dist
      }

      if (dist === 0) // if no casting is necessary, that's clearly the best
        break
    }
  }

  return [ bestDef, bestCasts ]
}

/**
 * Basic arithmetic operations TODO: extended names, etc.
 */

/**
 * Integer operations
 */

registerOperator(new OperatorDefinition({
  name: '+',
  args: ["int", "int"],
  returns: "int",
  evaluators: [
    new ConcreteEvaluator({
      args: [ "int", "int" ],
      returns: "int",
      primitive: "+"
    })
  ]
}))

registerOperator(new OperatorDefinition({
  name: '*',
  args: ["int", "int"],
  returns: "int",
  evaluators: [
    new ConcreteEvaluator({
      args: [ "int", "int" ],
      returns: "int",
      primitive: "*"
    })
  ]
}))

registerOperator(new OperatorDefinition({
  name: '-',
  args: ["int", "int"],
  returns: "int",
  evaluators: [
    new ConcreteEvaluator({
      args: [ "int", "int" ],
      returns: "int",
      primitive: "-"
    })
  ]
}))

// Unary minus
registerOperator(new OperatorDefinition({
  name: '-',
  args: ["int"],
  returns: "int",
  evaluators: [
    new ConcreteEvaluator({
      args: [ "int" ],
      returns: "int",
      primitive: "-"
    })
  ]
}))

/**
 * Real operators
 */

registerOperator(new OperatorDefinition({
  name: '+',
  args: ["real", "real"],
  returns: "real",
  evaluators: [
    new ConcreteEvaluator({
      args: [ "real", "real" ],
      returns: "real",
      primitive: "+"
    })
  ]
}))

registerOperator(new OperatorDefinition({
  name: '*',
  args: ["real", "real"],
  returns: "real",
  evaluators: [
    new ConcreteEvaluator({
      args: [ "real", "real" ],
      returns: "real",
      primitive: "*"
    })
  ]
}))

registerOperator(new OperatorDefinition({
  name: '/',
  args: ["real", "real"],
  returns: "real",
  evaluators: [
    new ConcreteEvaluator({
      args: [ "real", "real" ],
      returns: "real",
      primitive: "/"
    })
  ]
}))

registerOperator(new OperatorDefinition({
  name: '-',
  args: ["real", "real"],
  returns: "real",
  evaluators: [
    new ConcreteEvaluator({
      args: [ "real", "real" ],
      returns: "real",
      primitive: "-"
    })
  ]
}))

// Unary minus
registerOperator(new OperatorDefinition({
  name: '-',
  args: ["real"],
  returns: "real",
  evaluators: [
    new ConcreteEvaluator({
      args: [ "real" ],
      returns: "real",
      primitive: "-"
    })
  ]
}))

registerOperator(new OperatorDefinition({
  name: '^',
  args: ["real", "real"],
  returns: "real",
  evaluators: [
    new ConcreteEvaluator({
      args: [ "real", "real" ],
      returns: "real",
      func: Math.pow
    })
  ]
}))

registerOperator(new OperatorDefinition({
  name: 'pow',
  args: ["real", "real"],
  returns: "real",
  evaluators: [
    new ConcreteEvaluator({
      args: [ "real", "real" ],
      returns: "real",
      func: Math.pow
    })
  ]
}))

/**
 * Complex
 */

// Constructors
registerOperator(new OperatorDefinition({
  name: 'complex',
  args: [ "real" ],
  returns: "complex",
  evaluators: [
    new ConcreteEvaluator({
      args: [ "real" ],
      returns: "complex",
      evalType: "new",
      func: x => new Complex(x, 0)
    }),
    new ConcreteEvaluator({
      args: [ "real" ],
      returns: "complex",
      evalType: "write",
      func: (x, dst) => {
        dst.re = x
        dst.im = 0
      }
    })
  ]
}))

registerOperator(new OperatorDefinition({
  name: 'complex',
  args: [ "real", "real" ],
  returns: "complex",
  evaluators: [
    new ConcreteEvaluator({
      args: [ "real", "real" ],
      returns: "complex",
      evalType: "new",
      func: (x, y) => new Complex(x, y)
    }),
    new ConcreteEvaluator({
      args: [ "real", "real" ],
      returns: "complex",
      evalType: "write",
      func: (x, y, dst) => {
        dst.re = x
        dst.im = y
      }
    })
  ]
}))

registerOperator(new OperatorDefinition({
  name: '+',
  args: ["complex", "complex"],
  returns: "complex",
  evaluators: [
    new ConcreteEvaluator({
      args: [ "complex", "complex" ],
      returns: "complex",
      evalType: "new",
      func: (x, y) => {
        return new Complex(x.re + y.re, x.im + y.im)
      }
    }),
    new ConcreteEvaluator({
      args: [ "complex", "complex" ],
      returns: "complex",
      evalType: "write",
      func: (x, y, dst) => {
        dst.re = x.re + y.re
        dst.im = x.im + y.im
      }
    })
  ]
}))

registerOperator(new OperatorDefinition({
  name: '*',
  args: ["complex", "complex"],
  returns: "complex",
  evaluators: [
    new ConcreteEvaluator({
      args: [ "complex", "complex" ],
      returns: "complex",
      evalType: "new",
      func: (x, y) => {
        return new Complex(x.re * y.re - x.im * y.im, x.re * y.im + x.im * y.re)
      }
    }),
    new ConcreteEvaluator({
      args: [ "complex", "complex" ],
      returns: "complex",
      evalType: "write",
      func: (x, y, dst) => {
        dst.re = x.re * y.re - x.im * y.im
        dst.im = x.re * y.im + x.im * y.re
      }
    })
  ]
}))

registerOperator(new OperatorDefinition({
  name: '/',
  args: ["complex", "complex"],
  returns: "complex",
  evaluators: [
    new ConcreteEvaluator({
      args: [ "complex", "complex" ],
      returns: "complex",
      evalType: "write",
      func: (z1, z2, dst) => dst.divide(z1, z2)
    })
  ]
}))

registerOperator(new OperatorDefinition({
  name: '-',
  args: ["complex", "complex"],
  returns: "complex",
  evaluators: [
    new ConcreteEvaluator({
      args: [ "complex", "complex" ],
      returns: "complex",
      evalType: "write",
      func: (z1, z2, dst) => dst.subtract(z1, z2)
    })
  ]
}))

// Unary minus
registerOperator(new OperatorDefinition({
  name: '-',
  args: ["complex"],
  returns: "complex",
  evaluators: [
    new ConcreteEvaluator({
      args: [ "complex" ],
      returns: "complex",
      evalType: "write",
      func: (z, dst) => { dst.re = -z.re; dst.im = -z.im }
    })
  ]
}))

registerOperator(new OperatorDefinition({
  name: '^',
  args: ["complex", "complex"],
  returns: "complex",
  evaluators: [
    new ConcreteEvaluator({
      args: [ "complex", "complex" ],
      returns: "complex",
      evalType: "write",
      func: (c1, c2, dst) => dst.pow(c1, c2)
    })
  ]
}))

/**
 * Real/imag components
 */

registerOperator(new OperatorDefinition({
  name: 'Re',
  args: ["complex"],
  returns: "real",
  evaluators: [
    new ConcreteEvaluator({
      args: ["complex"],
      returns: "real",
      func: c => c.re
    })
  ]
}))

registerOperator(new OperatorDefinition({
  name: 'Im',
  args: ["complex"],
  returns: "real",
  evaluators: [
    new ConcreteEvaluator({
      args: ["complex"],
      returns: "real",
      func: c => c.im
    })
  ]
}))

/**
 * Special functions
 */

registerOperator(new OperatorDefinition({
  name: 'gamma',
  args: ["real"],
  returns: "real",
  evaluators: [
    new ConcreteEvaluator({
      args: ["real"],
      returns: "real",
      func: gammaReal
    })
  ]
}))

registerOperator(new OperatorDefinition({
  name: 'gamma',
  args: ["complex"],
  returns: "complex",
  evaluators: [
    new ConcreteEvaluator({
      args: ["complex"],
      returns: "complex",
      evalType: "write",
      func: (z, dst) => dst.gamma(z)
    })
  ]
}))


registerOperator(new OperatorDefinition({
  name: 'cos',
  args: ["complex"],
  returns: "complex",
  evaluators: [
    new ConcreteEvaluator({
      args: [ "complex" ],
      returns: "complex",
      evalType: "write",
      func: (z, dst) => dst.cos(z)
    })
  ]
}))

registerOperator(new OperatorDefinition({
  name: 'sin',
  args: ["complex"],
  returns: "complex",
  evaluators: [
    new ConcreteEvaluator({
      args: [ "complex" ],
      returns: "complex",
      evalType: "write",
      func: (z, dst) => dst.sin(z)
    })
  ]
}))
