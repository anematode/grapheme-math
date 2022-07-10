import { ConcreteCast, registerConcreteCast } from '../concrete_evaluator.js'
import { MathematicalCast, registerMathematicalCast } from '../operator_definition.js'
import { Complex } from '../../complex/normal.js'

let intToReal = [
  registerConcreteCast(new ConcreteCast({
    src: "int",
    dst: "real",
    identity: true
  })),
  registerConcreteCast(new ConcreteCast({
    src: "interval_int",
    dst: "interval_real",
    identity: true
  }))
]

let realToComplex = [
  registerConcreteCast(new ConcreteCast({
    src: "real", dst: "complex",
    evalType: "write", func: (src, dst) => { dst.re = src; dst.im = 0; }
  })),
  registerConcreteCast(new ConcreteCast({
    src: "real", dst: "complex",
    evalType: "new", func: r => new Complex(r, 0)
  }))
]

let intToComplex = [
  registerConcreteCast(new ConcreteCast({
    src: "int", dst: "complex",
    evalType: "write", func: (src, dst) => { dst.re = src; dst.im = 0; }
  })),
  registerConcreteCast(new ConcreteCast({
    src: "int", dst: "complex",
    evalType: "new", func: r => new Complex(r, 0)
  }))
]


  registerMathematicalCast(new MathematicalCast({
    src: "int",
    dst: "real",
    evaluators: intToReal
  }))

  registerMathematicalCast(new MathematicalCast({
    src: "int",
    dst: "complex",
    evaluators: intToComplex
  }))

  registerMathematicalCast(new MathematicalCast({
    src: "real",
    dst: "complex",
    evaluators: realToComplex
  }))
