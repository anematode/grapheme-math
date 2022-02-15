import {isRoundingMode, ROUNDING_MODE} from "../rounding_modes.js"
import {leftZeroPad} from "../../grapheme_shared.js"
import {
  flrLog2,
  getExponentAndMantissa,
  getFloatStoreExponent,
  getFloatStoreMantissa,
  isDenormal,
  pow2,
  setFloatStore
} from "../fp/manip.js"

// A float is of the following form: sign * (2^30)^e * m, where m is a list of 30-bit words that contain the mantissa of
// the float. m = m_1 / 2^30 + m_2 / 2^60 + ... . The precision is the number of bits kept track of in the words. Since
// the start of the significant bits can occur anywhere from 0 to 29 bits into the first word, we may need some extra
// space. Because this system is meant for high-performance calculations, I'm a bit more annoying about keeping things
// super optimized, even if that means dealing with JS's interesting behaviors.

// More details: e is between BIGFLOAT_MIN_EXP and BIGFLOAT_MAX_EXP, inclusive. sign is how special values are actually
// kept tracked of, and BigFloats are like IEEE floats in that they have signed zeros, infinity, and NaN. So, sign may
// take on the values 1, -1, +0, -0, NaN, Infinity, and -Infinity. The latter six are special.

// Mantissas are Int32Arrays, but store 30-bit words. A valid mantissa must have each entry be between 0 and
// BIGFLOAT_WORD_MAX, inclusive. The first word of a mantissa (in a non-zero, finite float) must be nonzero, so that
// each float has a unique representation (shifting the exponent by one is like shifting the whole mantissa by one
// word). Mantissas are generally passed around with their lengths included, for efficiency and for allowing mantissas
// to only be partially used.

const BIGFLOAT_WORD_BITS = 30
const BIGFLOAT_WORD_SIZE = 1 << BIGFLOAT_WORD_BITS
const BIGFLOAT_WORD_MAX = BIGFLOAT_WORD_SIZE - 1

const BIGFLOAT_MAX_EXP = Number.MAX_SAFE_INTEGER
const BIGFLOAT_MIN_EXP = -BIGFLOAT_MAX_EXP

const BIGFLOAT_MIN_PRECISION = 4 // in bits
const BIGFLOAT_MAX_PRECISION = 1 << 24 // can be changed later

const BIGFLOAT_MAX_MANTISSA_LEN = neededWordsForPrecision(BIGFLOAT_MAX_PRECISION) // Mantissas larger than this are impossible

const recip2Pow30 = 9.313225746154785e-10 // 2^-30
const recip2Pow60 = 8.673617379884035e-19 // 2^-60

// Default precision and rounding mode values
let WORKING_PRECISION = 53
let WORKING_RM = ROUNDING_MODE.NEAREST

/**
 * Set the working precision. Silently fails if the precision is invalid
 * @param p {number}
 */
export function setWorkingPrecision (p) {
  if (precisionInRange(p))
    WORKING_PRECISION = p
}

/**
 * Get the working precision.
 * @returns {number}
 */
export function getWorkingPrecision () {
  return WORKING_PRECISION
}

/**
 * Set the working rounding mode. Silently fails if the rounding mode is invalid
 * @param rm
 */
export function setWorkingRM (rm) {
  if (isRoundingMode(rm))
    WORKING_RM = rm
}

/**
 * Get the working rounding mode
 * @returns {number}
 */
export function getWorkingRM () {
  return WORKING_RM
}

/**
 * The minimum number of words needed to store a mantissa with prec bits. The +2 is because the bits need to be stored
 * at any shift within the word, from 1 to 29, so some space may be needed. +2 means that no matter the precision, the
 * end of precision will never be the exact end of the mantissa. For example,
 *
 * prec = 30, neededWords = 2              --> end of precision
 * m = [ 1, 0b111111111111111111111111111111 ]
 *       <------------- prec ------------->
 * prec = 31, neededWords = 3                 --> end of precision
 * m = [ 1, 0b111111111111111111111111111111, 0 ]
 *       <------------- prec -------------->
 * @param prec {number}
 * @returns {number}
 */
export function neededWordsForPrecision (prec) {
  prec |= 0

  return ((prec - 1) / BIGFLOAT_WORD_BITS + 2) | 0
}

// Whether a precision is valid
function precisionInRange (prec) {
  return typeof prec === "number" && prec >= BIGFLOAT_MIN_PRECISION && prec <= BIGFLOAT_MAX_PRECISION
}

// Throws if a precision is invalid
function precisionInRangeThrows (prec) {
  if (typeof prec !== "number" || prec < BIGFLOAT_MIN_PRECISION || prec > BIGFLOAT_MAX_PRECISION)
    throw new RangeError(`Precision must be a number in range [${BIGFLOAT_MIN_PRECISION}, ${BIGFLOAT_MAX_PRECISION}], not ${prec}`)
}

function createMantissa (prec) {
  return new Int32Array(neededWordsForPrecision(prec))
}

/**
 * Throws if a mantissa is invalid, with a reason
 * @param m {Int32Array}
 * @param mLen {number} Length of the mantissa; defaults to the mantissa's total length
 */
export function validateMantissa (m, mLen=-1) {
  if (!(m instanceof Int32Array)) throw new TypeError("Mantissa must be an Int32Array")
  if (mLen === -1) mLen = m.length

  if (m.length === 0 || mLen > BIGFLOAT_MAX_MANTISSA_LEN)
    throw new RangeError(`Mantissa has length ${mLen}, must be between [1, ${BIGFLOAT_MAX_MANTISSA_LEN}]`)

  if (m[0] === 0) throw new Error(`First word of mantissa is zero`)
  for (let i = 0; i < mLen; ++i) {
    let word = m[i]
    if (word < 0 || word > BIGFLOAT_WORD_MAX)
      throw new RangeError(`Word at index ${i} is ${toHex(word)}, outside of valid range [0, ${BIGFLOAT_WORD_MAX}]`)
  }
  // ok
}

/**
 * Checks whether a BigFloat is valid and throws if not
 * @param f {BigFloat}
 */
export function validateBigFloat (f) {
  if (!(f instanceof BigFloat)) throw new TypeError("f is not a BigFloat")
  let { sign, exp, prec, mant } = f

  precisionInRangeThrows(prec)
  let neededLen = neededWordsForPrecision(prec)
  if (mant?.length < neededLen) {
    throw new Error(`Float mantissa has length ${mant.length}, needs to have at least length ${neededLen} to handle precision ${prec}`)
  }

  if (Number.isFinite(sign) && sign !== 0 && sign !== 1 && sign !== -1) throw new RangeError(`Sign ${sign} is invalid`)
  if (exp > BIGFLOAT_MAX_EXP || exp < BIGFLOAT_MIN_EXP) throw new RangeError(`Exponent ${exp} is outside valid range [${BIGFLOAT_MIN_EXP}, ${BIGFLOAT_MAX_EXP}]`)

  if (!f.isSpecial()) {
    validateMantissa(f.mant)
  }
  // ok
}

/**
 * Compute the signed error, in ulps, between two mantissas. This error is given as a fraction of ulps. m2 is err ulps
 * greater than m1. The ulp is determined by m1, the reference mantissa, not m2
 * @param m1 {Int32Array}
 * @param mLen {number}
 * @param m2 {Int32Array}
 * @param m2shift {number} m2 is shifted to the right by this many words
 * @param tLen {number}
 * @param prec {number}
 */
export function ulpError (m1, mLen, m2, m2shift, tLen, prec) {
  let offset = Math.clz32(m[0]) - 2
  let endOfPrecision = (prec + offset) | 0
  let endOfPrecisionWordI = (endOfPrecision / BIGFLOAT_WORD_BITS) | 0

  if (endOfPrecisionWordI >= mLen) {
    // End of precision occurs after the reference mantissa

  }

}

/**
 * Round an (unsigned) mantissa to a given precision, in one of a few rounding modes. Also returns a shift if the
 * rounding operation brings the float to a higher exponent. Trailing information may be provided about the digits
 * following the mantissa to ensure correct rounding in those cases. This function allows aliasing, meaning the target
 * mantissa and the given mantissa can be the same array, leading to an in-place operation. Trailing information about
 * the mantissa may be provided for correct rounding (are tbhe words beyond the end of the mantissa negative, zero, or
 * positive?)
 * @param m {Int32Array}
 * @param mLen {number} Treated length of the mantissa (assumed correct)
 * @param t {Int32Array} Target mantissa (may be aliased to m)
 * @param tLen {number} Treated length of the target (assumed correct)
 * @param prec {number} Precision (target mantissa length assumed to be sufficient)
 * @param rm {number} Rounding mode (assumed to be valid)
 * @param trailing {number} Trailing information about the mantissa. 0 -> all zeros, 1 -> positive
 * @return {number} The shift, in words, of the new mantissa
 */
export function roundMantissaToPrecision (m, mLen, t, tLen, prec, rm, trailing=0) {
  mLen = mLen | 0
  tLen = tLen | 0
  prec = prec | 0
  rm = rm | 0
  trailing = trailing | 0

  if (rm === 0) {
    // rounding mode whatever
    if (m !== t) {
      // just need to copy over m to t (if there is no aliasing)
      let len = mLen > tLen ? tLen : mLen
      for (let i = 0; i < len; ++i) t[i] = m[i]
      // We don't even need to clear the rest of t to zeros, because it's rounding mode whatever
    }

    return 0
  }

  let offset = Math.clz32(m[0]) - 2
  let trunc = (prec + offset) | 0                   // what BIT after which the mantissa should be all zeros
  let truncWordI = (trunc / BIGFLOAT_WORD_BITS) | 0 // which word this bit occurs in
  // How many bits needs to be removed off the word; always between 1 and 30 inclusive
  let truncLen = BIGFLOAT_WORD_BITS - (trunc - Math.imul(truncWordI, BIGFLOAT_WORD_BITS) /* will never overflow */)

  /**
   * Examples:
   * m = [ 0x02345678, 0x12345678, 0x12345678 ], prec = 53
   * offset = 4, trunc = 57, truncWord = 1, truncLen = 3
   *                                             v truncWord              ---> everything past here must be 0
   * m = [ 0b000010001101000101011001111000, 0b010010001101000101011001111000, 0b010010001101000101011001111000 ]
   *         <--><-------------------------------------------------------><->
   *    offset = 4                   precision = 53                     truncLen = 3
   *
   * Common edge case: truncation occurs at a word boundary
   * m = [ 0x02345678, 0x12345678, 0x12345678 ], prec = 56
   * offset = 4, trunc = 60, truncWord = 2, truncLen = 30
   *                                                                             --> everything past here must be 0
   * m = [ 0b000010001101000101011001111000, 0b010010001101000101011001111000, 0b010010001101000101011001111000 ]
   *         <--><---------------------------------------------------------->    <---------------------------->
   *    offset = 4                   precision = 56                                 truncLen = 30
   */

  // because we assumed that tLen is large enough for prec, we know that truncWord lies within [0, tLen - 1]. Note that
  // because there is always at least one bit of extra space, the end of precision is never the end of a mantissa (see
  // neededWordsForPrecision for details)

  if (truncWordI >= mLen) {
    // End of precision occurs after the given mantissa, so the entire mantissa fits in the target; copy it over
    if (m !== t) for (let i = 0; i < mLen; ++i) t[i] = m[i]

    // clear rest of target. the trailing info is not enough to recover any information
    for (let i = mLen; i < tLen; ++i) t[i] = 0
    return 0
  }

  // The word to be truncated, its truncated form, and how much will be removed
  let word = m[truncWordI]
  let truncWord = (word >> truncLen) << truncLen
  let rem = word - truncWord

  // true if we just truncate; false if we round up at the end of precision
  let doTruncation = true

  // The rounding mode now matters
  if (rm & 2) { // ties
    let tieSplit = 1 << (truncLen - 1)
    if (rem < tieSplit) {
      doTruncation = true
    } else if (rem > tieSplit) {
      doTruncation = false
    } else {
      // Try to break the tie; any nonzero word implies a round up
      if (trailing === 1)
        doTruncation = false
      else for (let i = truncWordI + 1; i < mLen; ++i) {
          if (m[i] > 0) {
            doTruncation = false
            break
          }
        }

        /**
         * Example of a tie with ties to even or odd:
         * m = [ 0x02345678, 0x12345678, 0x20000000 ], prec = 56
         * offset = 4, trunc = 60, truncWord = 2, truncLen = 30
         *                                                                        |
         *                                              examine the preceding bit v    ---> everything past here must be 0
         * m = [ 0b000010001101000101011001111000, 0b010010001101000101011001111000, 0b100000000000000000000000000000 ]
         *         <--><---------------------------------------------------------->    <---------------------------->
         *    offset = 4                   precision = 56                                     truncLen = 30
         *
         * Note that truncLen = 30 has to be handled specially. Because we are tying to even and the preceding bit is 0,
         * we truncate instead of rounding up
         */

        if (doTruncation) { // True tie. We either tie up, tie down, tie to even, or tie to odd.
          if (rm === ROUNDING_MODE.TIES_AWAY) doTruncation = false
          else if (rm === ROUNDING_MODE.TIES_ZERO) doTruncation = true
          else {
            // We examine the bit preceding the end of precision
            let lastBit = truncLen === 30 ? (m[truncWordI - 1] & 1) : truncWord & (1 << truncLen)
            if (lastBit) doTruncation = rm === ROUNDING_MODE.TIES_ODD
            else doTruncation = rm === ROUNDING_MODE.TIES_EVEN
          }
        }
      }
  } else if (!(rm & 1)) { // toward inf or up
    if (rem > 0) doTruncation = false
    else {
      // any nonzero word means we don't truncate
      if (trailing) doTruncation = false
      else for (let i = truncWordI + 1; i < mLen; ++i) {
        if (m[i] > 0) {
          doTruncation = false
          break
        }
      }
    }
  } else {
    // Truncate no matter what
    doTruncation = true
  }

  // Now that we know whether to truncate or carry up, we copy over the relevant words from the source to the target
  // mantissa and truncate or carry
  if (m !== t) for (let i = 0; i < truncWordI; ++i) t[i] = m[i]

  let shift = 0  // shift will be 1 if everything carries over
  if (!doTruncation) {
    // Carry up one bit
    truncWord = (truncWord + (1 << truncLen)) | 0
    if (truncWord > BIGFLOAT_WORD_MAX) {
      // We need to do a carry!
      truncWord = 0

      let i = truncWordI - 1
      for (; i >= 0; --i) {
        let word = t[i] + 1
        t[i] = word

        // We keep carrying until the word is not larger than 0x3fffffff
        if (word > BIGFLOAT_WORD_MAX) t[i] = 0
        else break
      }

      if (i === -1) {
        // We carried all the way! The mantissa is now all zeros, but should be [ 0x00000001, 0 ... ] with a shift of 1
        // (increment the exponent by 1)

        t[0] = 1
        shift = 1
      }
    }
  }

  t[truncWordI] = truncWord
  for (let i = truncWordI + 1; i < tLen; ++i) t[i] = 0 // clear the remainder of target

  return shift
}

class BigFloat {
  constructor (sign, exp, prec, mant) {
    this.sign = sign
    this.exp = exp
    this.prec = prec
    this.mant = mant
  }

  /**
   * Create a BigFloat, initialized to zero, of a given precision
   * @param prec {number}
   * @returns {BigFloat}
   */
  static new (prec=WORKING_PRECISION) {
    precisionInRangeThrows(prec)

    let mant = createMantissa(prec)
    return new BigFloat(0, 0, prec, mant)
  }

  /**
   * Create a BigFloat from a JS number, rounding in the given direction. Special numbers will be preserved
   * @param n {number} Any JS number
   * @param prec {number} Precision
   * @param rm {number} Rounding mode
   * @returns {BigFloat}
   */
  static fromNumber (n, prec=WORKING_PRECISION, rm=WORKING_RM) {
    let f = BigFloat.new(prec)
    f.setFromNumber(n, rm)

    return f
  }

  /**
   * Convert this BigFloat to a normal JS number, rounding in the given direction and optionally rounding to the nearest
   * float32 value. That functionality is more given to verify the double logic. It *does* handle denormal numbers,
   * unfortunately for me.
   * @param rm {number}
   * @param f32 {boolean} Whether to cast to a float32 instead of a float64
   * @returns {number}
   */
  toNumber (rm = WORKING_RM, f32 = false) {
    if (this.isSpecial()) return this.sign

    let m = this.mant, unshiftedExp = (this.exp - 1) * BIGFLOAT_WORD_BITS, mLen = m.length // exp in base 2

    if (!rm) {
      // Rounding mode whatever: Short-circuit calculation for efficiency
      let m0 = m[0], m1 = m[1], m2 = (mLen < 3) ? 0 : m[2]

      return pow2(unshiftedExp) * (m0 + m1 * recip2Pow30 + m2 * recip2Pow60)
    }

    let prec = f32 ? 24 : 53
    let roundedMantissa = SCRATCH_MANTISSA

    if (rm & 16 && this.sign === -1) rm ^= 1 // flip rounding mode for sign

    // Round to the nearest float32 or float64, ignoring denormal numbers for now
    let shift = roundMantissaToPrecision(m, mLen, roundedMantissa, 3, prec, rm)

    let MIN_EXPONENT = f32 ? -149 : -1074
    let MIN_NORMAL_EXPONENT = f32 ? -126 : -1022
    let MAX_EXPONENT = f32 ? 127 : 1023
    let MIN_VALUE = f32 ? 1.175494e-38 : Number.MIN_VALUE
    let MAX_VALUE = f32 ? 3.40282347e38 : Number.MAX_VALUE

    // Calculate an exponent and mant such that mant * 2^exponent = the number
    let mAsInt = 0, expShift = 0, exp = 0, denormal = false
    do {
      if (shift) {
        mAsInt = 1 << 30
      } else {
        mAsInt =
          roundedMantissa[0] +
          roundedMantissa[1] * recip2Pow30 +
          (f32 ? 0 : roundedMantissa[2] * recip2Pow60) // because the mantissa is rounded, this is exact
      }

      // Normalize mant to be in the range [0.5, 1)
      expShift = flrLog2(mAsInt) + 1
      mAsInt /= pow2(expShift)
      exp = unshiftedExp + expShift

      if (exp < MIN_NORMAL_EXPONENT && exp >= MIN_EXPONENT && !denormal) {
        // denormal, round to a different precision
        shift = roundMantissaToPrecision(m, mLen, roundedMantissa, 3, exp - MIN_EXPONENT, rm)
        denormal = true // go back and calculate mAsInt
      } else break
    } while (denormal)

    // If the exponent is outside of bounds, we clamp it to a value depending on the rounding mode
    if (exp < MIN_EXPONENT) {
      if (rm & 2) { // tie
        // Deciding between 0 and MIN_VALUE. Unfortunately at 0.5 * 2^-1074 there is a TIE omg
        if (exp === MIN_EXPONENT - 1) {
          // If greater or ties away
          if (mAsInt > 0.5 || (rm === ROUNDING_MODE.TIES_AWAY
            || rm === ROUNDING_MODE.TIES_ODD)) {
            return this.sign * MIN_VALUE
          }
        }

        return this.sign * 0
      } else {
        if (!(rm & 1)) { // ties up or inf; note that rm has been flipped already to correspond to the magnitude
          return this.sign * MIN_VALUE
        } else { // down
          return this.sign * 0
        }
      }
    } else if (exp > MAX_EXPONENT) {
      if (exp === MAX_EXPONENT + 1) {
        // Bottom formula will overflow, so we adjust
        return this.sign * mAsInt * 2 * pow2(exp - 1)
      }

      // 2^(MAX_EXPONENT + 1) ties between MAX_VALUE and Infinity. Ties to even, toward inf gives the latter; ties to
      // odd, toward zero gives the former
      if (exp === MAX_EXPONENT + 2 && mAsInt === 0.5) {
        if (rm === ROUNDING_MODE.TIES_EVEN || rm === ROUNDING_MODE.TIES_AWAY) return this.sign * Infinity
        else return this.sign * MAX_VALUE
      }

      if (rm & 1) {
        // ties down in magnitude or toward zero
        return this.sign * MAX_VALUE
      } else {
        return this.sign * Infinity
      }
    } else {
      return this.sign * mAsInt * pow2(exp)
    }
  }

  /**
   * Set the value of this BigFloat from a BigFloat, keeping this float's precision.
   * @param f
   * @param rm
   * @returns {BigFloat} This, for chaining
   */
  setFromBigFloat (f, rm=WORKING_RM) {
    if (!(f instanceof BigFloat)) throw new TypeError("BigFloat.setFromBigFloat takes a BigFloat")

    let fs = f.sign
    this.sign = fs

    if (fs === 0 || !Number.isFinite(fs)) return this

    let fm = f.mant, fml = fm.length, fe = f.exp, tm = this.mant, tml = tm.length
    let shift = roundMantissaToPrecision(fm, fml, tm, tml, this.prec, rm) // copy over

    let e = shift + fe
    if (e > BIGFLOAT_MAX_EXP) {
      // overflow
      this.sign = Infinity
    } else {
      this.exp = e
    }

    return this
  }

  /**
   * Set the value of this BigFloat from a JS number. TODO: make more efficient
   * @param n {number}
   * @param rm {number} Rounding mode to be used; only relevant if prec < 53
   * @returns {BigFloat}
   */
  setFromNumber (n, rm=WORKING_RM) {
    if (this.prec < 53) {
      // Weird and rare case. Rounding to a lower precision is needed

      SCRATCH_DOUBLE.setFromNumber(n)
      this.setFromBigFloat(SCRATCH_DOUBLE)
      return this
    }

    if (typeof n !== "number") throw new TypeError("BigFloat.setFromNumber takes a JS number")
    n = +n

    const mant = this.mant  // mant.length guaranteed >= 3

    let nDenormal = isDenormal(n)
    setFloatStore(n)
    let valExponent = getFloatStoreExponent(n)
    let valMantissa = getFloatStoreMantissa(n)

    // Exponent of the float (2^30)^newExp
    let newExp = Math.ceil((valExponent + 1) / BIGFLOAT_WORD_BITS)

    // The mantissa needs to be shifted to the right by this much. 0 < bitshift <= 30. If the number is denormal, we
    // have to shift it by one bit less
    let bitshift = newExp * BIGFLOAT_WORD_BITS - valExponent - nDenormal
    let denom = pow2(bitshift + 22)
    mant[0] =
      Math.floor(valMantissa / denom) /* from double */ +
      (nDenormal ? 0 : 1 << (30 - bitshift)) /* add 1 if not denormal */

    let rem = valMantissa % denom
    if (bitshift > 8) {
      let cow = 1 << (bitshift - 8)

      mant[1] = Math.floor(rem / cow)
      mant[2] = rem % cow << (38 - bitshift)
    } else {
      mant[1] = rem << (8 - bitshift)
      mant[2] = 0
    }

    // Special handling; for tiny denormal numbers, the first word is 0, so we shift them over
    if (nDenormal && mant[0] === 0) {
      mant[0] = mant[1]
      mant[1] = mant[2]
      mant[2] = 0

      newExp -= 1
    }

    // Clear mantissa
    for (let i = mant.length - 1; i >= 3; --i) mant[i] = 0

    this.exp = newExp
    this.sign = Math.sign(n)

    return this
  }

  /**
   * Whether this number is ±0, NaN, or ±inf and therefore is treated specially
   * @returns {boolean}
   */
  isSpecial () {
    return !Number.isFinite(this.sign) || this.sign === 0
  }
}

const SCRATCH_MANTISSA = createMantissa(53)
const SCRATCH_DOUBLE = BigFloat.new(53)

// Convenience functions

/**
 * Convert a number to a hex representation of exactly 8 digits
 * @param a {number}
 * @returns {string}
 */
export function toHex (a) {
  return ((a < 0) ? '-' : '') + "0x" + leftZeroPad(Math.abs(a).toString(16), 8, '0')
}

export function toBinary (a) {
  return ((a < 0) ? '-' : '') + "0b" + leftZeroPad(Math.abs(a).toString(2), 30, '0')
}

/**
 * Pretty print a mantissa for analysis
 * @param mantissa {Int32Array}
 * @param color {string} Optional, of the form \x1b[32m, etc; used for command-line prettifying
 * @param binary {string} Whether to display it as 30-bit padded binary
 * @returns {string}
 */
export function prettyPrintMantissa (mantissa, color="", binary=false) {
  return '[ ' + Array.from(mantissa).map(binary ? toBinary : toHex)
    .map(s => `${color}${s}${color ? "\x1b[0m" : ''}`).join(', ') + ' ]'
}


export { BigFloat }
