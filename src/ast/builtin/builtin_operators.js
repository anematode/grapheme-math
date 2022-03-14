import { OperatorDefinition } from "../operator_definition.js"
import { castDistance, ConcreteEvaluator } from '../evaluator.js'
import {gammaReal} from "../../real/normal.js"

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
