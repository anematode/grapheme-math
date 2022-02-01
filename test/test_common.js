export const NONFINITE_NUMBERS = [ NaN, Infinity, -Infinity ]

export const PATHOLOGICAL_NUMBERS = [
  0, 1, -0, -1, 5e-324 /* smallest positive denormal */,
  ...NONFINITE_NUMBERS,
  -5e-324, Number.MAX_VALUE, -Number.MAX_VALUE, Number.MAX_SAFE_INTEGER, -Number.MAX_SAFE_INTEGER,
  2.2250738585072009e-308 /* smallest positive normal */, -2.2250738585072009e-308,
  2.2250738585072004e-308 /* largest denormal */, -2.2250738585072004e-308
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
