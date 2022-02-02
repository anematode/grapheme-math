import {difficultMantissas, typicalMantissas} from "../../test_common.js"
import {addMantissas, neededWordsForPrecision} from "../../../src/arb/bigfloat.js"

let precisions = [153, 153, 153, 153, 153]
let roundingModes = [0, 1, 2, 5]

let cases = 0
let start = Date.now()

difficultMantissas.push(...typicalMantissas)

let target = new Int32Array(Math.max(neededWordsForPrecision(153), 5))
for (let i = 0; i < difficultMantissas.length; ++i) {
  const m1 = difficultMantissas[i]
  for (const m2 of difficultMantissas) {
    for (let shift = 0; shift < 5; ++shift) {
      for (let targetSize = 1; targetSize < 5; ++targetSize) {
        for (let p = 0; p < precisions.length; ++p) {
          let precision = precisions[p]
          for (let r = 0; r < roundingModes.length; ++r) {
            let roundingMode = roundingModes[r]
            addMantissas(m1, m1.length, m2, m2.length, shift, precision, target, target.length, roundingMode)
            cases++
          }
        }
      }
    }
  }

  (!(i % 10)) ? console.log(`Progress: ${(i / difficultMantissas.length * 100).toPrecision(4)}% complete`) : 0
}

let end = Date.now()

console.log("nspt", (end - start) / cases * 1e6, "cases", cases)
