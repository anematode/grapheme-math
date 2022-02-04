import {RANDOM_NUMBERS} from "../../test_common.js"

export function getExponent (x) {
  const floatStore = new Float64Array(1)
  const intView = new Int32Array(floatStore.buffer)

  floatStore[0] = x

  // Mask the biased exponent, retrieve it and convert it to non-biased
  return ((intView[1] & 0x7ff00000) >> 20) - 1023
}

let n = 0
for (let i = 0; i < 1e3; ++i) {
  for (let j = 0; j < RANDOM_NUMBERS.length; ++j) {
    n += getExponent(RANDOM_NUMBERS[j])
  }
}
