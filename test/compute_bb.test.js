import { WASM } from "../build/algorithm/wasm/wasm_wrapper.js"
import { _boundingBoxFlatF32 } from "../build/algorithm/miscellaneous_geometry.js"
import {expect} from "chai"
import {expectMultipleCases} from "./test.js"
import {BoundingBox} from "../build/other/bounding_box.js"
import {roundUp} from "../build/fp/manip.js"

const wasmImpl = WASM.boundingBoxFlatF32
const jsImpl = _boundingBoxFlatF32

describe("computeBoundingBox", () => {
  it("Correctly ignores NaN values", () => {
    const cases = [
      [[[ NaN, NaN ]], null],
      [[[]], null],
      [[[ 1, 2, 4, 10, 50, 20, 10, -50, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN]], new BoundingBox(1, -50, roundUp(49), roundUp(70))]
    ]
    expectMultipleCases(wasmImpl, cases)
    expectMultipleCases(jsImpl, cases)
  })
})
