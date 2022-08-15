import {rightZeroPad} from "../build/utils.js"
import {ROUNDING_MODE} from "../build/other/rounding_modes.js"

export const NONFINITE_NUMBERS = [ NaN, Infinity, -Infinity ]

export const PATHOLOGICAL_NUMBERS = [
  0, 1, -0, -1, 5e-324 /* smallest positive denormal */,
  ...NONFINITE_NUMBERS,
  -5e-324, Number.MAX_VALUE, -Number.MAX_VALUE, Number.MAX_SAFE_INTEGER, -Number.MAX_SAFE_INTEGER,
  2.2250738585072009e-308 /* smallest positive normal */, -2.2250738585072009e-308,
  2.2250738585072004e-308 /* largest denormal */, -2.2250738585072004e-308,
  1.401298464324817e-45 /* smallest positive f32 */, -1.401298464324817e-45,
  7.006492321624085e-46 /* half of smallest positive f32 */, -7.006492321624085e-46,
]

let a = new Float64Array(1)
let b = new Uint32Array(a.buffer)

// Randomly generated from across the whole float range
export const RANDOM_NUMBERS = []

/**
 * Handles -0 specially
 * @param d {number}
 * @returns {string}
 */
export function pedanticToString(d) {
  return Object.is(d, -0) ? '-0' : (d + '')
}

;((rng) => {
  for (let i = 0; i < 1000; ++i) {
    b[0] = rng()
    b[1] = rng()

    RANDOM_NUMBERS.push(a[0])
  }
})(getRNG(10))

// somewhat more concentrated around 1
export const TYPICAL_NUMBERS = RANDOM_NUMBERS.map(d => {
  let k = Math.log2(Math.abs(d))
  return Math.cbrt(Math.sin(k) ** 30 * k * Math.cbrt(d))
})

export const ROUNDING_MODES = Object.values(ROUNDING_MODE)
export const STRICT_ROUNDING_MODES = ROUNDING_MODES.filter(s => s !== 0)

// Credit to https://github.com/bryc, https://stackoverflow.com/a/47593316/13458117
// Generates in [0, 2^32 - 1]
function getRNG (seed=0) {
  let h = seed

  return function() {
    h = Math.imul(h ^ (h >>> 16), 2246822507);
    h = Math.imul(h ^ (h >>> 13), 3266489909);
    return (h ^= h >>> 16) >>> 0;
  }
}

// Garden variety mantissas
export const typicalMantissas = []

// List of mantissas that are likely to cause trouble
export const difficultMantissas = []

// Mantissas consisting of only ones
export const mantissaAllOnes = []

function mantissaFromBinaryString (str) {
  let arr = []

  for (let i = 0; i < str.length; i += 30) {
    arr.push(parseInt(rightZeroPad(str.slice(i, i + 30), 30), 2))
  }

  return arr
}

for (let i = 0; i < 29; ++i) {
  for (let count = 0; count < 100; ++count) {
    let str = '0'.repeat(i) + '1'.repeat(count)

    mantissaAllOnes.push(mantissaFromBinaryString(str))
  }
}


// Mantissas containing various troublesome words like 0x20000000 and 0x1fffffff
const troublesomeWords = [ 0x20000000, 0x1fffffff, 0x00000000, 0x00000001, 0x3fffffff, 0x1ffffffe, 0x20000001 ]

troublesomeWords.forEach(w => w ? difficultMantissas.push([ w ]) : 0)
troublesomeWords.forEach(w1 => w1 ? troublesomeWords.forEach(w2 => difficultMantissas.push([ w1, w2 ])) : 0)
troublesomeWords.forEach(w1 => w1 ? troublesomeWords.forEach(w2 => troublesomeWords.forEach(w3 => difficultMantissas.push([ w1, w2, w3 ]))) : 0)

const typicalWords = [ 0x20000000, 0x2eefcafe, 0x32ab30ca, 0x00000000, 0x00000040, 0x0024feef ]

typicalWords.forEach(w => w ? typicalMantissas.push([ w ]) : 0)
typicalWords.forEach(w1 => w1 ? typicalWords.forEach(w2 => typicalMantissas.push([ w1, w2 ])) : 0)
typicalWords.forEach(w1 => w1 ? typicalWords.forEach(w2 => typicalWords.forEach(w3 => typicalMantissas.push([ w1, w2, w3 ]))) : 0)

// Credit to https://stackoverflow.com/a/43053803/13458117
export function cartesianProduct (...args) {
  return args.reduce((a, b) => a.flatMap(d => b.map(e => [d, e].flat(1))))
}

export const ALL_NUMBERS = [ [...new Array(1000).keys()].map(x => [x, -x]).flat(), ...TYPICAL_NUMBERS, ...RANDOM_NUMBERS, ...PATHOLOGICAL_NUMBERS ]
export const RANDOM_BIGINTS = []

let rng = getRNG(11)
let k = 14204n

for (let i = -1000; i < 1000; ++i) {
  RANDOM_BIGINTS.push(BigInt(i))

  k >>= 20n;
  k += BigInt(rng())

  k *= BigInt(rng())
  RANDOM_BIGINTS.push(k)
  RANDOM_BIGINTS.push(-k)
}

