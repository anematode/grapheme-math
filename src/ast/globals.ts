

// Constants are treated as operators, since they are mathematical in nature and may have any number of concrete impls.

import {OperatorDefinition} from "./operator_definition.js"
import {ConcreteEvaluator} from "./evaluator.js"
import {Complex} from "../complex/normal.js"

export const MathematicalConstants = {
  pi: new OperatorDefinition({
    name: "const_pi",
    args: [],
    returns: "real",
    constant: true,
    evaluators: [
      new ConcreteEvaluator({
        args: [],
        returns: "real",
        func: () => Math.PI
      })
    ]
  }),
  e: new OperatorDefinition({
    name: "const_e",
    args: [],
    returns: "real",
    constant: true,
    evaluators: [
      new ConcreteEvaluator({
        args: [],
        returns: "real",
        func: () => Math.E
      })
    ]
  }),
  i: new OperatorDefinition({
    name: "const_i",
    args: [],
    returns: "complex",
    constant: true,
    evaluators: [
      new ConcreteEvaluator({
        args: [],
        returns: "complex",
        func: () => new Complex(0, 1)
      })
    ]
  })
}
