// Used for bit-level manipulation of floats
export const floatStore = new Float64Array(1)
export const intView = new Uint32Array(floatStore.buffer)

const POSITIVE_NORMAL_MIN = 2.2250738585072014e-308
const NEGATIVE_NORMAL_MAX = -POSITIVE_NORMAL_MIN
const POSITIVE_DENORMAL_MIN = Number.MIN_VALUE
const MAX_VALUE = Number.MAX_VALUE

const MAGIC_ROUND_C = 1.1113332476497816e-16 // just above machine epsilon / 2

/**
 * Returns the next floating point number after x. For example, roundUp(0) returns Number.MIN_VALUE.
 * inf -> inf, -inf -> -min negative value, nan -> nan, -0, 0 -> min positive denormal, max negative denormal -> 0. This
 * function is pretty darn fast and if it's inlined, is probably 2-4 ns / call.
 * @param f Any floating-point number
 * @returns The next representable floating-point number, handling special cases
 */
export function roundUp (f: number): number {
  if (f >= -POSITIVE_NORMAL_MIN && f < POSITIVE_NORMAL_MIN) {
    // denormal numbers
    return f + POSITIVE_DENORMAL_MIN
  } else if (f === -Infinity) {
    // special case
    return -MAX_VALUE
  }

  return f + Math.abs(f) * MAGIC_ROUND_C
}

/**
 * Returns the previous floating point number before x. Equivalent to -roundUp(-x)
 * @param x Any floating-point number
 * @returns The previous representable floating-point number, handling special cases
 */
export function roundDown (x: number): number {
  if (x > -POSITIVE_NORMAL_MIN && x <= POSITIVE_NORMAL_MIN) {
    return x - POSITIVE_DENORMAL_MIN
  } else if (x === Infinity) {
    return MAX_VALUE
  }

  return x - Math.abs(x) * MAGIC_ROUND_C
}

/**
 * Return whether a number is denormal; see {@link https://en.wikipedia.org/wiki/Denormal_number|Wikipedia} for a
 * technical explanation of what that means. ±0 are not considered denormal.
 * @param x Any floating-point number
 * @returns Whether the number is a denormal number
 */
export function isDenormal (x: number): boolean {
  // Note that NaN will return false, since NaN < anything is false.
  return x !== 0 && x < POSITIVE_NORMAL_MIN && x > NEGATIVE_NORMAL_MAX
}

/**
 * Get the non-biased exponent of a floating-point number x. Equivalent *mathematically* to floor(log2(abs(x))) for
 * finite values--but only if you're using infinite precision.
 * @param x Any floating-point number
 * @returns The non-biased exponent of that number's floating-point representation
 */
export function getExponent (x: number): number {
  floatStore[0] = x

  // Mask the biased exponent, retrieve it and convert it to non-biased
  return ((intView[1] & 0x7ff00000) >> 20) - 1023
}

/**
 * Get the mantissa of a floating-point number as an integer in [0, 2^53).
 * @param x Any floating-point number
 * @returns An integer in [0, 2^53)
 */
export function getMantissa (x: number): number {
  floatStore[0] = x

  return intView[0] + _getMantissaHighWord() * 4294967296
}

/**
 * Get the exponent and mantissa of a number
 * @param x
 */
export function getExponentAndMantissa (x: number): [ number, number ] {
  floatStore[0] = x

  return [
    getFloatStoreExponent(),
    getFloatStoreMantissa()
  ]
}

function _getMantissaHighWord () {
  return intView[1] & 0x000fffff
}


/**
 * Set the internal float store value to be manipulated
 * @param x Any floating-point number
 */
export function setFloatStore (x: number) {
  floatStore[0] = x
}

/**
 * Get the stored exponent after calling setFloatStore
 */
export function getFloatStoreExponent (): number {
  return ((intView[1] & 0x7ff00000) >> 20) - 1023
}

/**
 * Get the stored mantissa after calling setFloatStore
 */
export function getFloatStoreMantissa (): number {
  return intView[0] + _getMantissaHighWord() * 4294967296
}

/**
 * Testing function counting the *approximate* number of floats between x1 and x2, including x1 but excluding x2. NaN if
 * either is undefined. It is approximate because the answer may sometimes exceed Number.MAX_SAFE_INTEGER, but it is
 * exact if the answer is less than Number.MAX_SAFE_INTEGER.
 * @param x1 The lesser number
 * @param x2 The greater number
 * @returns The number of floats in the interval [x1, x2)
 */
export function countFloatsBetween (x1: number, x2: number): number {
  if (Number.isNaN(x1) || Number.isNaN(x2)) {
    return NaN
  }

  if (x1 === x2) return 0

  if (x2 < x1) {
    const tmp = x1
    x1 = x2
    x2 = tmp
  }

  const [x1man, x1exp] = frExp(x1)
  const [x2man, x2exp] = frExp(x2)

  return (x2man - x1man) * 2 ** 53 + (x2exp - x1exp) * 2 ** 52
}

const pow2Lookup = new Float64Array(2098)
let e = Number.MIN_VALUE
for (let i = -1074; i <= 1023; ++i) {
  pow2Lookup[i + 1074] = e
  e *= 2
}

/**
 * Calculates 2 ^ exp, using a customized method for integer exponents. An examination of fdlibm's pow function didn't
 * reveal any special handling, and indeed my benchmark indicates this method is 3 times faster than pow for integer
 * exponents. Note that bit shifts can't really be used except for a restricted range of exponents.
 * @param exp Exponent; intended for use with integers, but permits any floating-point number.
 * @returns Returns 2 ^ exp, and is guaranteed to be exact for integer exponents.
 */
export function pow2 (exp: number): number {
  if (!Number.isInteger(exp)) return Math.pow(2, exp)
  if (exp > 1023) return Infinity
  if (exp < -1074) return 0

  exp |= 0

  return pow2Lookup[exp + 1074]
}

// Counts the number of trailing zeros in a 32-bit integer n; similar to <i>Math.clz32</i>.
function countTrailingZeros (n: number): number {
  let bits = 0

  if (n !== 0) {
    let x = n

    // Suck off groups of 16 bits, then 8 bits, et cetera
    if ((x & 0x0000ffff) === 0) {
      bits += 16
      x >>>= 16
    }

    if ((x & 0x000000ff) === 0) {
      bits += 8
      x >>>= 8
    }

    if ((x & 0x0000000f) === 0) {
      bits += 4
      x >>>= 4
    }

    if ((x & 0x00000003) === 0) {
      bits += 2
      x >>>= 2
    }

    bits += (x & 1) ^ 1
  } else {
    return 32
  }

  return bits
}

function _mantissaCtz (): number {
  const bits = countTrailingZeros(intView[0])

  if (bits === 32) {
    const secondWordCount = countTrailingZeros(_getMantissaHighWord())

    return 32 + Math.min(secondWordCount, 20)
  }

  return bits
}

/**
 * Counts the number of trailing zeros in the mantissa of a floating-point number, between 0 and 52.
 * @param d {number} A floating-point number
 * @returns {number} The number of trailing zeros in that number's mantissa
 */
export function mantissaCtz (d: number): number {
  floatStore[0] = d

  return _mantissaCtz()
}

function _mantissaClz (): number {
  const bits = Math.clz32(_getMantissaHighWord()) - 12 // subtract the exponent zeroed part

  return bits !== 20 ? bits : bits + Math.clz32(intView[0])
}

/**
 * Counts the number of leading zeros in the mantissa of a floating-point number, between 0 and 52.
 * @param d A floating-point number
 * @returns The number of leading zeros in that number's mantissa
 */
export function mantissaClz (d: number): number {
  floatStore[0] = d

  return _mantissaClz()
}

/**
 * Converts a floating-point number into a fraction in [0.5, 1) or (-1, -0.5], except special cases, and an exponent,
 * such that fraction * 2 ^ exponent gives the original floating point number. If x is ±0, ±Infinity or NaN, [x, 0] is
 * returned to maintain this guarantee.
 * @param x Any floating-point number
 * @returns [fraction, exponent]
 */
export function frExp (x: number): [ number, number ] {
  if (x === 0 || !Number.isFinite(x)) return [x, 0]

  // +1 so that the fraction is between 0.5 and 1 instead of 1 and 2
  let exp = getExponent(x) + 1

  // Denormal
  if (exp === -1022) {
    // If the mantissa is the integer m, then we should subtract clz(m) from exp to get a suitable answer
    exp -= _mantissaClz()
  }

  return [x / pow2(exp), exp]
}

/**
 * Converts a floating-point number into a numerator, denominator and exponent such that it is equal to n/d * 2^e. n and
 * d are guaranteed to be less than or equal to 2^53 and greater than or equal to 0 (unless the number is ±0, Infinity,
 * or NaN, at which point [x, 1, 0] is returned). See Grapheme Theory for details. n/d is between 0.5 and 1.
 *
 * TODO: optimize
 * @param x Any floating-point number
 * @returns [numerator, denominator, exponent]
 */
export function rationalExp (x: number): [ number, number, number ] {
  let [frac, denExponent, exp] = rationalExpInternal(x)
  let den = pow2(denExponent)

  return [frac * den, den, exp]
}

function rationalExpInternal (x: number): [ number, number, number ] {
  if (x < 0) {
    const [num, den, exp] = rationalExpInternal(-x)

    return [-num, den, exp]
  }

  if (x === 0 || !Number.isFinite(x)) return [x, 0, 0]

  // Decompose into frac * 2 ^ exp
  const [frac, exp] = frExp(x)

  // This tells us the smallest power of two which frac * (2 ** shift) is an integer, which is the denominator
  // of the dyadic rational corresponding to x
  const denExponent = 53 - mantissaCtz(frac)

  return [frac, denExponent, exp]
}

/**
 * Converts a floating-point number into an integer and exponent [i, e], so that i * 2^e gives the original number. i
 * will be within the bounds of Number.MAX_SAFE_INTEGER.
 * @param x Any floating-point number
 */
export function integerExp (x: number): [ number, number ] {
  const [frac, denExponent, exp] = rationalExpInternal(x)

  return [frac * pow2(denExponent), exp - denExponent]
}

/**
 * Compute an accurate floor log 2 function. Note that Math.log2 is not good enough here;
 * floor(log2(268435455.99999994)), for example, returns 28 when the mathematical value is 27.
 * @param x Any floating-point number
 */
export function flrLog2 (x: number): number {
  let exp = getExponent(x) + 1

  if (exp === -1022) exp -= _mantissaClz() // denormal

  return exp - 1
}

/**
 * Compute the unit in the last place for a given floating-point number. Returns NaN if the number is infinite and the
 * smallest positive denormal if it is equal to 0.
 * @param x Any floating-point number
 */
export function ulp (x: number): number {
  if (!Number.isFinite(x)) return Infinity
  if (x === 0) return Number.MIN_VALUE

  let exp = getExponent(x)
  if (exp === -1022) return Number.MIN_VALUE // denormal

  return pow2(exp - 52)
}
