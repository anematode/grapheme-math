import {isRoundingMode, ROUNDING_MODE, RoundingMode} from "../other/rounding_modes.js"
import {leftZeroPad} from "../utils.js"
import { flrLog2, getFloatStoreExponent, getFloatStoreMantissa, isDenormal, pow2, setFloatStore } from "../fp/manip.js"
import {getTrailingInfo2, rightShiftMantissa} from "./old.js";

// A float is of the following form: sign * (2^30)^e * m, where m is a list of 30-bit words that contain the mantissa of
// the float. m = m_1 / 2^30 + m_2 / 2^60 + ... . The precision is the number of bits kept track of in the words. Since
// the start of the significant bits can occur anywhere from 0 to 29 bits into the first word, we may need some extra
// space. Because this system is meant for high-performance calculations, I'm a bit more annoying about keeping things
// well-optimized, even if that means dealing with JS's interesting behaviors.

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

const BIGFLOAT_MAX_MANTISSA_LEN = neededWordsForPrecision(BIGFLOAT_MAX_PRECISION) // Mantissas larger than this are prohibited

const recip2Pow30 = 9.313225746154785e-10 // 2^-30
const recip2Pow60 = 8.673617379884035e-19 // 2^-60

// Default precision and rounding mode values
let WORKING_PRECISION: number = 53
let WORKING_RM: RoundingMode = ROUNDING_MODE.NEAREST

type Mantissa = Int32Array

/**
 * Set the working precision. Silently fails if the precision is invalid
 * @param p {number}
 */
export function setWorkingPrecision (p: number) {
  if (precisionInRange(p))
    WORKING_PRECISION = p
}

/**
 * Get the working precision.
 * @returns {number}
 */
export function getWorkingPrecision (): number {
  return WORKING_PRECISION
}

/**
 * Set the working rounding mode. Silently fails if the rounding mode is invalid
 * @param rm
 */
export function setWorkingRM (rm: RoundingMode) {
  if (isRoundingMode(rm))
    WORKING_RM = rm
}

/**
 * Get the working rounding mode
 */
export function getWorkingRM (): RoundingMode {
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
 * @param prec
 * @returns
 */
export function neededWordsForPrecision (prec: number): number {
  prec |= 0

  return ((prec - 1) / BIGFLOAT_WORD_BITS + 2) | 0
}

// Whether a precision is valid
function precisionInRange (prec: number): boolean {
  return typeof prec === "number" && prec >= BIGFLOAT_MIN_PRECISION && prec <= BIGFLOAT_MAX_PRECISION
}

// Throws if a precision is invalid
function precisionInRangeThrows (prec: number) {
  if (typeof prec !== "number" || prec < BIGFLOAT_MIN_PRECISION || prec > BIGFLOAT_MAX_PRECISION)
    throw new RangeError(`Precision must be a number in range [${BIGFLOAT_MIN_PRECISION}, ${BIGFLOAT_MAX_PRECISION}], not ${prec}`)
}

function createMantissa (prec: number): Int32Array {
  return new Int32Array(neededWordsForPrecision(prec))
}

/**
 * Throws if a mantissa is invalid, with a reason
 * @param m
 * @param mLen Length of the mantissa; defaults to the mantissa's total length
 */
export function validateMantissa (m: Mantissa, mLen: number=-1) {
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
export function validateBigFloat (f: BigFloat) {
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
  let offset = Math.clz32(m1[0]) - 2
  let endOfPrecision = (prec + offset) | 0
  let endOfPrecisionWordI = (endOfPrecision / BIGFLOAT_WORD_BITS) | 0

  if (endOfPrecisionWordI >= mLen) {
    // End of precision occurs after the reference mantissa

  }

}

/**
 * Given a subarray of a mantissa, return 0 if infinite zeros; 1 if between 0 and 0.5; 2 if a tie; 3 if between a tie
 * and 1. The stuff beyond the mantissa is considered to be all zeros. This is useful when rounding. As an example,
 * mant = [ 0x3fffffff, 00000000, 00000001 ]
 *                      ^ index       gives 1.
 * mant = [ 0x3fffffff, 20000000, 00000001 ]
 *                      ^ index       gives 3.
 * mant = [ 0x3fffffff, 20000000, 00000000 ]
 *                      ^ index       gives 2.
 * This function only supports all-positive mantissas.
 * @param mantissa {Int32Array}
 * @param index {number} From which index (not bit!) to search
 * @returns {number}
 */
export function getTrailingInfo (mantissa: Mantissa, index: number): number {
  let mantissaLen = mantissa.length

  if (index >= 0) {
    if (index < mantissaLen) {
      if (mantissa[index] === 1 << 29) {
        // Potential tie
        for (let i = index + 1; i < mantissaLen; ++i) {
          if (mantissa[i] !== 0) return 3
        }
        return 2
      } else if (mantissa[index] > 1 << 29) {
        return 3
      }
    } else { // index < mantissaLen
      return 0
    }
  } else { // index >= 0
    index = 0
  }

  for (let i = index; i < mantissa.length; ++i) {
    if (mantissa[i] !== 0) return 1
  }

  return 0
}


/**
 * Left shift a mantissa by shift bits, destroying any bits that come off the front, writing the result to target.
 * This function supports aliasing.
 */
export function leftShiftMantissa (m: Mantissa, mLen: number, shift: number, t: Mantissa, tLen: number) {
  if (shift === 0) {
    if (t !== m) {
      let copyLen = Math.min(tLen, mLen)

      for (let i = copyLen; i >= 0; --i) {
        t[i] = m[i]
      }

      for (let i = tLen - 1; i > copyLen; --i) {
        t[i] = 0
      }
    }
  }

  let integerShift = (shift / 30) | 0
  let bitShift = shift % 30

  if (bitShift === 0) {
    // Since it's a multiple of 30, we just copy everything over
    for (let i = integerShift; i < mLen; ++i) {
      t[i - integerShift] = m[i]
    }

    // Fill empty stuff with zeros
    for (let i = mLen - integerShift; i < tLen; ++i) {
      t[i] = 0
    }
  } else {
    let invBitShift = 30 - bitShift

    for (let i = integerShift; i < mLen; ++i) {
      t[i - integerShift] =
          ((m[i] << bitShift) & 0x3fffffff) +
          (i < mLen - 1 ? m[i + 1] >> invBitShift : 0)
    }

    for (let i = mLen - integerShift; i < tLen; ++i) {
      t[i] = 0
    }
  }
}

/**
 * roundMantissaToPrecision, but permitting an invalid input mantissa with leading zero words. If all words are zero,
 * the target mantissa is untouched and a shift of 0 is returned.
 */
export function roundMantissaToPrecisionWithSeek(m: Mantissa, mLen: number, t: Mantissa, tLen: number, prec: number, rm: RoundingMode, trailing: number=0): number {
  let seekShift = 0

  seek: {
    for (let i = 0; i < mLen; ++i) {
      if (m[i] !== 0) {
        seekShift = -i

        leftShiftMantissa(m, mLen, 30 * i, m, mLen)
        break seek
      }
    }

    return 0;   // mantissa is all zeros :/
  }

  return seekShift + roundMantissaToPrecision(m, mLen, t, tLen, prec, rm, trailing)
}

/**
 * Round an (unsigned) mantissa to a given precision, in one of a few rounding modes. Also returns a shift if the
 * rounding operation brings the float to a higher exponent. Trailing information may be provided about the digits
 * following the mantissa to ensure correct rounding in those cases. This function allows aliasing, meaning the target
 * mantissa and the given mantissa can be the same array, leading to an in-place operation. Trailing information about
 * the mantissa may be provided for correct rounding (are the words beyond the end of the mantissa negative, zero, or
 * positive?)
 * @param m {Int32Array}
 * @param mLen {number} Treated length of the mantissa (assumed correct)
 * @param t {Int32Array} Target mantissa (may be aliased to m)
 * @param tLen {number} Treated length of the target (assumed correct)
 * @param prec {number} Precision (target mantissa length assumed to be sufficient)
 * @param rm {number} Rounding mode (assumed to be valid)
 * @param trailing {number} Trailing information about the mantissa. 0 -> all zeros, 1 -> between 0 and 0.5, 2 -> tie (0.5), 3 -> greater than 0.5
 * @return {number} The shift, in words, of the new mantissa
 */
export function roundMantissaToPrecision (m: Mantissa, mLen: number, t: Mantissa, tLen: number, prec: number, rm: RoundingMode, trailing: number=0): number {
  mLen = mLen | 0
  tLen = tLen | 0
  prec = prec | 0
  rm = (rm | 0) as RoundingMode
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

  let shift = 0  // shift will be 1 if everything carries over
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
    return shift
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
      if (trailing >= 1)
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
        shift += 1
      }
    }
  }


  t[truncWordI] = truncWord
  for (let i = truncWordI + 1; i < tLen; ++i) t[i] = 0 // clear the remainder of target

  return shift
}

/**
 * Compare two mantissas. Returns -1 if m1 is smaller, 0 if they are equal, and 1 if m1 is greater.
 * @param m1 First mantissa
 * @param m1Len Effective length of first mantissa
 * @param m2 Second mantissa
 * @param m2Len Effective length of second mantissa
 */
export function compareMantissas (m1: Mantissa, m1Len: number, m2: Mantissa, m2Len: number) {
  let swapResult = false
  if (m1Len < m2Len) {
    let tmp = m1
    m1 = m2
    m2 = tmp

    swapResult = true
  }

  let result = 0
  for (let i = 0; i < m1Len; ++i) {
    let mant1Word = m1[i]
    let mant2Word = i < m2Len ? m2[i] : 0

    if (mant1Word > mant2Word) {
      result = 1
      break
    } else if (mant1Word < mant2Word) {
      result = -1
      break
    }
  }

  return swapResult ? -result : result
}

/**
 * Add two mantissas together, potentially with an integer word shift on the second mantissa. The result mantissa may
 * also have a shift applied to it, which is relative to mant1. This function seems like it would be relatively simple,
 * but the shifting brings annoyingness, especially with the rounding modes. The overall concept is we compute as much
 * of the addition as needed without doing any carrying, then when we get to the end of the area of needed precision,
 * we continue computing until we can determine with certainty the carry and the rounding direction. This function
 * allows aliasing mant1 to be the target mantissa. TODO optimize
 * @param m1 {Int32Array}
 * @param m1l {number} Length of the first mantissa; how many words to actually use
 * @param m2 {Int32Array} Nonnegative shift applied to mantissa 2
 * @param m2l {number} Length of the second mantissa; how many words to actually use
 * @param m2shift {number} Number of words by which the second mantissa is shifted to the right (must be >= 0)
 * @param prec {number} Precision to compute and round to
 * @param t {Int32Array} The mantissa that is written to
 * @param tLen {number} Number of words in the target mantissa
 * @param rm {number} Rounding mode
 */
export function addMantissas (m1: Mantissa, m1l: number, m2: Mantissa, m2l: number, m2shift: number, t: Mantissa, tLen: number, prec: number, rm: RoundingMode): number {
  let isAliased = m1 === t
  let mant2End = m2l + m2shift

  let newMantLen = tLen
  let newMant = t

  // Need to compute to higher precision first (TODO not necessary)
  if (m1l > newMantLen) {
    let neededWords = neededWordsForPrecision(prec)
    newMantLen = m1l > neededWords ? m1l : neededWords
    newMant = new Int32Array(newMantLen)
  }

  // We first copy over all the parts of the addition we definitely need:
  if (!isAliased) {
    for (let i = 0; i < m1l; ++i) {
      newMant[i] = m1[i]
    }

    for (let i = m1l; i < newMantLen; ++i) {
      newMant[i] = 0
    }
  }

  let mant2Bound1 = mant2End < newMantLen ? mant2End : newMantLen
  for (let i = m2shift; i < mant2Bound1; ++i) {
    newMant[i] += m2[i - m2shift]
  }

  // Do the carry
  let carry = 0
  for (let i = m1l - 1; i >= 0; --i) {
    let word = newMant[i] + carry

    if (word > 0x3fffffff) {
      word -= 0x40000000
      newMant[i] = word
      carry = 1
    } else {
      newMant[i] = word
      carry = 0
    }
  }

  // All that remains are the words of mant2 to the right of newMantLen - mant2Shift
  let trailingInfo = 0
  let needsTrailingInfo = (rm & 2) || (!(rm & 1)) // ties or inf/up

  if (needsTrailingInfo) {
    let trailingShift = newMantLen - m2shift
    trailingInfo = getTrailingInfo(m2, trailingShift > 0 ? trailingShift : 0)

    // If the trailing info is shifted, then round it to 0 or 1 as appropriate
    if (trailingShift < 0) trailingInfo = +!!trailingInfo
  }

  let shift = 0

  if (carry) {
    // Get trailing info from beyond the end of the truncation due to right shifting LOL
    if (needsTrailingInfo) {
      let lastWord = newMant[newMantLen - 1]

      if (lastWord === 0) {
        trailingInfo = +!!trailingInfo
      } else if (lastWord < 0x20000000) {
        trailingInfo = 1
      } else if (lastWord === 0x20000000) {
        trailingInfo = trailingInfo ? 3 : 2
      } else {
        trailingInfo = 3
      }
    }

    for (let i = newMantLen - 2; i >= 0; --i) newMant[i + 1] = newMant[i]

    newMant[0] = 1
    shift += 1
  }

  let roundingShift = rm === ROUNDING_MODE.WHATEVER ? 0 : roundMantissaToPrecision(
      newMant,
      newMantLen,
      t,
      tLen,
      prec,
      rm,
      trailingInfo
  )

  return roundingShift + shift
}

/**
 * Subtract two (positive) mantissas, with mant2 under a given shift, returning a bit field of the
 * following:
 * <------      0 / 1         0 / 1
 *   shift   equals zero  flipped sign
 * So if mant2 > mant1, (returnedShift & 1) === 1; if (mant2 === mant1), (returnedShift & 2) === 1. The net shift
 * relative to the larger mantissa, in words, is returnedShift >> 2. If mant2 === mant1, the target mantissa is
 * UNTOUCHED.
 * @param m1 {Int32Array}
 * @param m1Len
 * @param m2 {Int32Array}
 * @param m2Len
 * @param m2shift {number}
 * @param t {Int32Array} The mantissa to write to
 * @param tLen
 * @param prec {number}
 * @param rm {number}
 */
export function subtractMantissas (m1: Mantissa,
                                   m1Len: number,
                                   m2: Mantissa,
                                   m2Len: number,
                                   m2shift: number,
                                   t: Mantissa,
                                   tLen: number,
                                   prec: number,
                                   rm: RoundingMode): number {
    // The algorithm for (efficient) subtraction is a bit complicated. The reference implementation allocates a mantissa
    // large enough to store the exact result, computes the exact result, and rounds it. But if mant2Shift is large, this
    // approach allocates and calculates way more stuff than it needs to. Ideally, we allocate little or nothing and only
    // compute as much as necessary for the target mantissa to be rounded to prec bits.

    // We already assume that mant1 and mant2 are both valid mantissas, and that mant1 > mant2. We need to find the
    // location of the first word of the result mantissa.

    let mant1Len = m1.length, mant2Len = m2.length, mant2End = mant2Len + m2shift, targetLen = t.length
    let exactEnd = Math.max(mant1Len, mant2End) // The end of exact computation, relative to the first word of mant1

    // We can visualize the situation as follows:
    //  <--           mant1Len = 4                  -->
    // [ 0xcafecafe, 0xcafecafe, 0xcafecafe, 0xcafecafe]
    //  <-- mant2Shift = 2 --> [ 0xdeadbeef, 0xdeadbeef, 0xdeadbeef ]
    //                          <--         mant2Len = 3         -->
    //  <--               exactEnd = mant2End = 5                -->

    // We calculate words of the result relative to the first word of mant1 (generally, this is how we index things). If a
    // word is 0, then the start of the result occurs later. If a word is 1, the start of the result may be there, or may
    // be later, depending on the next computed word: If the next computed word is negative, then the result begins later;
    // if the next computed word is 0, then the result may begin later; if the next computed word is positive, the result
    // begins at the word that is 1. If a word is 2 or greater, the start of the result is there.

    // mant1:      [ 0xcafecafe, 0xcafecafe, 0xcafecafe, 0x00000001 ]
    // mant2:      [ 0xcafecafe, 0xcafecafe, 0xcafecafd, 0x00000000 ]
    // computed words:    0           0           1       1 (positive)
    //                                            ^ result begins here

    // mant1:      [ 0xcafecafe, 0xcafecafe, 0xcafecafe, 0x00000000 ]
    // mant2:      [ 0xcafecafe, 0xcafecafe, 0xcafecafd, 0x00000001 ]
    // computed words:    0           0           1       -1 (negative)
    //                                            result begins later due to carry -->
    // after carry:[ 0x00000000, 0x00000000, 0x00000000, 0x3fffffff ]
    //                                                        ^ result begins here

    // In any case, once a positive word is discovered, we start storing computed words in the target mantissa. Once the
    // target mantissa is exhausted, we do the carry and count how many of the first n words are 0. If n > 0, we shift the
    // target mantissa left by n words and continue computing words, etc. etc. If n == 0, we note that the maximum
    // possible imprecision in the result is +-1 units in the last place (of the last word), so we check whether this
    // error bound is sufficient for us to call it quits under the current precision and rounding mode. If not, we must
    // compute the (positive or negative) trailing information of words following the target mantissa. In particular, we
    // need to know which range the stuff after the target lies in: (-0x40000000, -0x20000000), -0x20000000 (tie),
    // (-0x20000000, 0x00000000), 0x00000000 (zero), (0x00000000, 0x20000000), 0x20000000 (tie), (0x20000000, 0x40000000).
    // These cases are enumerated as -3, -2, -1, 0, 1, 2, and 3, respectively.

    let positiveComputedI = -1
    let positiveComputedWord = 0
    let wordIndex = 0

    for (; wordIndex < exactEnd; ++wordIndex) {
      let mant2Index = wordIndex - m2shift
      let mant1Word = (wordIndex < mant1Len) ? m1[wordIndex] : 0,
          mant2Word = (mant2Index >= 0 && mant2Index < mant2Len) ? m2[mant2Index] : 0

      let computedWord = mant1Word - mant2Word
      if (computedWord > 0) {
        positiveComputedI = wordIndex
        positiveComputedWord = computedWord
        break
      }
    }

    t[0] = positiveComputedWord

    // mant1:      [ 0xcafecafe, 0xcafecafe, 0xcafecafe, 0x00000000 ]
    // mant2:      [ 0xcafecafe, 0xcafecafe, 0xcafecafd, 0x00000001 ]
    // computed words:    0           0           1 = positiveComputedWord
    //                                            ^ positiveComputedWordIndex

    // Returns the number of zero words that occurred at the beginning of target
    function doCarry (startIndex = targetLen-1) {
      // Before: [ 0x00000002, -0x3fffffff, -0x3fffffff, -0x3ffffffe ]
      // After:  [ 0x00000001,  0x00000000,  0x00000000,  0x00000002 ]
      //          n = 0 zero words

      // Before: [ 0x00000001, -0x3fffffff, -0x3fffffff, -0x3ffffffe ]
      // After:  [ 0x00000000,  0x00000000,  0x00000000,  0x00000002 ]
      //          <--       n = 3 zero words        -->

      let carry = 0
      for (let i = startIndex; i >= 0; --i) {
        let computedWord = t[i] + carry

        if (computedWord < 0) {
          carry = -1
          computedWord += 0x40000000
        } else {
          carry = 0
        }

        t[i] = computedWord
      }

      let zeroWords = 0
      for (; zeroWords < targetLen && (!t[zeroWords]); ++zeroWords);

      // Before: [ 0x00000000,  0x00000000,  0x00000000,  0x00000002 ]
      //          <--       n = 3 zero words        -->
      // After:  [ 0x00000002,  0x00000000,  0x00000000,  0x00000000 ]
      if (zeroWords > 0) {
        // We shift the target left so the first positive word is at index 0, then adjust positiveComputedWordIndex
        leftShiftMantissa(t, tLen, 30 * zeroWords, t, tLen)
        positiveComputedI += zeroWords
      }

      return zeroWords
    }

    // Compute words after the first positive computed word, carrying whenever the target is exhausted
    for (wordIndex = positiveComputedI + 1; wordIndex < exactEnd; ++wordIndex) {
      let targetIndex = wordIndex - positiveComputedI
      if (targetIndex >= targetLen) {
        let zeroWords = doCarry()

        if (zeroWords !== 0) {
          // The carry caused zeros to appear at the beginning, so we continue computing words
          targetIndex = wordIndex - positiveComputedI
        } else {
          // The target is full of computed words, and the first word is positive
          break
        }
      }

      let mant2Index = wordIndex - m2shift
      let mant1Word = (wordIndex < mant1Len) ? m1[wordIndex] : 0,
          mant2Word = (mant2Index >= 0 && mant2Index < mant2Len) ? m2[mant2Index] : 0
      let computedWord = (mant1Word - mant2Word) | 0

      t[targetIndex] = computedWord
    }

    if (wordIndex === exactEnd) {
      // Target was large enough to store the exact result. We clear everything after the last word and round
      let lastIndex = wordIndex - positiveComputedI
      for (let i = lastIndex; i < targetLen; ++i) t[i] = 0

      doCarry(lastIndex - 1)

      return -positiveComputedI + roundMantissaToPrecision(t, tLen, t, tLen, prec, rm)
    } else {
      let canBeRounded = canMantissaBeRounded(t, tLen, prec, wordIndex, rm, 1, 1)
      let trailingInfo = 0

      if (!canBeRounded) {
        // Compute trailing info
        let nextWord = 0

        while (1) {
          let mant2Index = wordIndex - m2shift
          let mant1Word = (wordIndex < mant1Len) ? m1[wordIndex] : 0,
              mant2Word = (mant2Index >= 0 && mant2Index < mant2Len) ? m2[mant2Index] : 0

          nextWord = (mant1Word - mant2Word) | 0

          if (nextWord < 0) {
            t[targetLen - 1] -= 1
            nextWord += 0x40000000

            let zeros = doCarry(targetLen - 1)

            if (zeros) {
              // Rare edge case:
              // mant1:          [ 0x00000001, 0x00000000, 0x00000000 ]
              // mant2:          [ 0x00000000, 0x00000000, 0x00000001 ]
              // target before:  [ 0x00000001, 0x00000000 ]
              // nextWord before: -0x00000001
              // target after:   [ 0x3fffffff, 0x3fffffff ]

              t[targetLen - 1] = nextWord
              wordIndex++

              continue
            }
          }

          break
        }

        // Various splitting points for rounding. 0 and 2 require more words to be examined
        if (nextWord === 0x00000000) {
          trailingInfo = 0
        } else if (nextWord < 0x20000000) {
          trailingInfo = 1
        } else if (nextWord === 0x20000000) {
          trailingInfo = 2
        } else {
          trailingInfo = 3
        }

        if (!(trailingInfo & 1)) {
          // trailingInfo = 0 or 2
          ++wordIndex

          // If the second mantissa is wholly after the first mantissa, skip ahead to the second mantissa since all the
          // words in between will be 0
          if (m2shift > mant1Len)
            wordIndex = Math.max(wordIndex, m2shift)

          for (; wordIndex < exactEnd; ++wordIndex) {
            let mant2Index = wordIndex - m2shift
            let mant1Word = (wordIndex < mant1Len) ? m1[wordIndex] : 0,
                mant2Word = (mant2Index >= 0 && mant2Index < mant2Len) ? m2[mant2Index] : 0

            let word = (mant1Word - mant2Word) | 0

            if (word === 0) continue
            else if (word < 0) {
              if (trailingInfo === 2) {
                // Negative word on a tie means it's below a tie
                trailingInfo = 1
                break
              }

              // Subtract one ulp, then set trailing info to 3 (equivalent to "trailing mode -1")
              t[targetLen - 1]--
              let zeros = doCarry(targetLen - 1)

              if (zeros) t[targetLen - 1] = 0x3fffffff

              trailingInfo = 3
              break
            } else if (word > 0) {
              if (trailingInfo === 2) {
                trailingInfo = 3
              } else trailingInfo = 1
              break
            }
          }
        }
      }

      return -positiveComputedI + roundMantissaToPrecision(t, tLen, t, tLen, prec, rm, trailingInfo)
    }
  }

/**
 * Multiply a mantissa by an integer between 1 and 2^30 - 1, returning a new mantissa and a shift amount. The shift
 * amount is the number of words by which the new mantissa is shifted relative to the first (and is thus either 0 or 1).
 * @return The shift of the operation
 */
export function multiplyMantissaByInteger (
    m: Mantissa,
    mLen: number,
    int: number,
    t: Mantissa,
    tLen: number,
    rm: RoundingMode,
    prec: number
): number {
    let newMantissa = new Int32Array(neededWordsForPrecision(prec) + 1) // extra word for overflow

    // Decompose the given integer into two 15-bit words for the multiplication
    let word1Lo = int & 0x7fff
    let word1Hi = int >> 15

    let carry = 0
    for (let i = mLen - 1; i >= 0; --i) {
        // Multiply the word, storing the low part and tracking the high part
        let word = m[i]

        let word2Lo = word & 0x7fff
        let word2Hi = word >> 15

        let low = Math.imul(word1Lo, word2Lo),
            high = Math.imul(word1Hi, word2Hi)
        let middle = Math.imul(word2Lo, word1Hi) + Math.imul(word1Lo, word2Hi)

        low += ((middle & 0x7fff) << 15) + carry
        if (low > 0x3fffffff) {
            high += low >> 30
            low &= 0x3fffffff
        }

        high += middle >> 15

        newMantissa[i + 1] = low
        carry = high
    }

    newMantissa[0] = carry
    let shift = 1

    if (carry === 0) {
        // Shift left; there was no carry after all
        for (let i = 0; i < tLen - 1; ++i) {
            newMantissa[i] = newMantissa[i + 1]
        }

        newMantissa[tLen - 1] = 0
        shift -= 1
    }

    let roundingShift = roundMantissaToPrecision(
        t, tLen, t, tLen, prec, rm, 0
    )

    return shift + roundingShift
}

/**
 * Returns whether a mantissa can be correctly rounded, assuming a maximum error of maxNeg and maxPos in the last word.
 * This often allows rounding to happen before extra computation is requested. Assumes maxNeg < BIGFLOAT_WORD_MAX and
 * maxPos < BIGFLOAT_WORD_MAX. This function can handle mantissa with negative words after the uncertain word, which is
 * required because it is used in subtractMantissas.
 * Examples:
 *                               v end of precision is here, ±0x2
 * mant = [ 0x3fffffff, 0x00000000, 0x00000001 ], precision = 59, round = NEAREST, maxNeg = 0, maxPos = 1
 *                         ^ uncertainWord
 * Cannot be rounded, because we are rounding ties to even. If it were round up or down, it could be rounded.
 *                               v end of precision is here, ±0x2
 * mant = [ 0x3fffffff, 0x00000000, -0x00000001 ], precision = 59, round = NEAREST, maxNeg = 0, maxPos = 1
 *                         ^ uncertainWord
 * Can be rounded, because we are rounding ties. If it were round up or down, it could not be rounded
 *
 * @param mantissa {Int32Array}
 * @param precision {number}
 * @param uncertainWord {number}
 * @param round {number}
 * @param maxNeg {number}
 * @param maxPos {number}
 */
export function canMantissaBeRounded (
    mantissa: Mantissa,
    mLen: number,
    precision: number,
    uncertainWord: number,
    round: RoundingMode,
    maxNeg: number,
    maxPos: number
) {
  if (maxNeg === 0 && maxPos === 0) return true // trivial

  let zeros = Math.clz32(mantissa[0]) - 2

  let endOfPrec = zeros + precision
  let endWord = (endOfPrec / 30) | 0

  if (uncertainWord < endWord) {
    // mant = [ 0x3fffffff,         0x00000002 ], precision = 59
    //              ^ uncertainWord     ^ endWord
    // ANY change in the uncertain word will change the rounding
    return false
  }

  let mantissaLen = mantissa.length

  if (endWord >= mantissaLen) {
    // TODO
    return false
  }

  let truncateLen = 30 - (endOfPrec - endWord * 30)  // in [1, 30]
  let truncatedWord = (mantissa[endWord] >> truncateLen) << truncateLen
  // If we truncated to precision, this is the remainder
  let rem = mantissa[endWord] - truncatedWord

  // TODO rewrite mantissa code to avoid these redundant checks
  let isUp = round === ROUNDING_MODE.UP || round === ROUNDING_MODE.TOWARD_INF
  let isDown = round === ROUNDING_MODE.DOWN || round === ROUNDING_MODE.TOWARD_ZERO
  let isTies = round === ROUNDING_MODE.TIES_AWAY || round === ROUNDING_MODE.TIES_EVEN

  // We can round if adding maxPos to rem and subtracting maxNeg from rem both give the same result after roundMantissa.
  // Note that 1 << truncateLen is the last bit of precision, and 1 << (truncateLen - 1) is the bit after the last bit
  // of precision. truncateLen == 1 has to be handled specially in the latter case.
  //
  // rem is the current truncation amount. Let t be the tying behavior of the words past endWord: 0 if all zeros, 1 if
  // less than 0.5, 2 if exactly 0.5, 3 if above, -1 if greater than -0.5, -2 if exactly -0.5, -3 otherwise.
  //  mant = [ 0x3fffffff, 0x20000001, 0x20000000 ], precision = 58, truncateLen = 2, rem = 2
  //                          ^ endWord    ^ t = 2
  // Suppose we are rounding up. If there were no uncertainty, we'd get [ 0x3fffffff, 0x20000004, 0 ]. If maxNeg is 1
  // and maxPos is 0, then we can still safely round up, because [ 0x3fffffff, 0x20000000, 0x20000000 ] rounded up is
  // the same. If t <= 0, we cannot round up, because it might be exactly [ ... 0x20000000, 0 ] or [ ... 0x20000000, -x].
  // If maxNeg is 2 we cannot round up. If maxNeg is 0 and maxPos is 2, we can safely round up because 2 + 1 < 4. If maxPos is 3, we cannot
  // safely round up because it will go to [ 0x3fffffff, 0x20000004, 0x20000000 ], rounding to [ 0x3fffffff, 0x20000008,
  // 0 ]. Thus, if rem + maxPos = 1 << truncateLen, we can only round if t == 0.
  //
  // The basic strategy here is we determine what direction in which the mantissa WILL be rounded up, then check whether
  // the result would be different if rem were rem - maxNeg and rem + maxPos.
  //
  // Consider the value of rem and t, the trailing value. If we are rounding up, then we truncate if rem = 0 and t <= 0,
  // and round up otherwise. If we are rounding down, we truncate if rem = 0 and t >= 0, and round down otherwise. If we
  // are rounding ties to even and truncateLen > 1, we truncate if rem is in [0, 1 << (truncateLen - 1)),
  // rem == 1 << (truncateLen - 1) and t = 0, and round up otherwise. If we are rounding ties to inf, then the second
  // case is a round up. If truncateLen = 1, then rem = 0 and any value of maxNeg or maxPos will not be okay.

  if (round === ROUNDING_MODE.WHATEVER) {
    // TODO verify
    return maxPos + maxNeg < (1 << truncateLen)
  }

  let info = -5
  function getTrailingInfo () {
    return (info === -5) ? (info = getTrailingInfo2(mantissa, endWord + 1)) : info
  }

  if (truncateLen === 1) {
    // No matter what, the result might be different
    return false;
  }

  let min = rem - maxNeg, max = rem + maxPos

  if (isUp || isDown) {
    if (rem === 0) {
      if (min < -(1 << truncateLen) || (min < 0 && getTrailingInfo() >= 0) || (min === -(1 << truncateLen) && getTrailingInfo() < 0))
        return false

      return !(max > (1 << truncateLen) || (max > 0 && getTrailingInfo() <= 0) || (max === (1 << truncateLen) && getTrailingInfo() > 0))
    }

    if (min < 0) return false
    if (max > (1 << truncateLen)) return false
    if (min === 0 && getTrailingInfo() <= 0) return false
    if (max === (1 << truncateLen) && getTrailingInfo() >= 0) return false

    return true
  }

  // ties TODO: make less conservative and more efficient
  let tieSplit = 1 << (truncateLen - 1)

  return (rem > tieSplit) ? !(min < tieSplit || max > 3 * tieSplit) : !(min < -tieSplit || max > tieSplit)
}

// Slow function used as a fallback when accumulated error is too large
function slowExactMultiplyMantissas (m1: Mantissa, m1Len: number, m2: Mantissa, m2Len: number, t: Mantissa, tLen:number, prec: number,  rm: RoundingMode) {
  let arr = new Int32Array(m1Len + m2Len + 1)

  for (let i = m1Len; i >= 0; --i) {
    let mant1Word = m1[i] | 0  // bleh
    let mant1WordLo = mant1Word & 0x7fff
    let mant1WordHi = mant1Word >> 15

    let carry = 0,
        j = m2.length - 1
    for (; j >= 0; --j) {
      let mant2Word = m2[j] | 0
      let mant2WordLo = mant2Word & 0x7fff
      let mant2WordHi = mant2Word >> 15

      let low = Math.imul(mant1WordLo, mant2WordLo),
          high = Math.imul(mant1WordHi, mant2WordHi)
      let middle =
          (Math.imul(mant2WordLo, mant1WordHi) +
              Math.imul(mant1WordLo, mant2WordHi)) |
          0

      low += ((middle & 0x7fff) << 15) + carry + arr[i + j + 1]
      low >>>= 0

      if (low > 0x3fffffff) {
        high += low >>> 30
        low &= 0x3fffffff
      }

      high += middle >> 15

      arr[i + j + 1] = low
      carry = high
    }

    arr[i] += carry
  }

  let shift = 0

  if (arr[0] === 0) {
    leftShiftMantissa(arr, arr.length, 30, arr, arr.length)
    shift -= 1
  }

  shift += roundMantissaToPrecision(
      arr,
      arr.length,
      t,
      t.length,
      prec,
      rm
  )

  return shift
}

export function multiplyMantissas(m1: Mantissa, m1Len: number, m2: Mantissa, m2Len: number, t: Mantissa, tLen: number, prec: number, rm: RoundingMode): number {
    for (let i = 0; i < tLen; ++i) t[i] = 0

    let highestWord = 0

    // Low words that weren't counted on the first pass. Note that this number may overflow the 32 bit integer limit
    let ignoredLows = 0
    let maxI = Math.min(tLen, m1Len - 1)

    // Only add the products whose high words are within targetMantissa
    for (let i = maxI; i >= 0; --i) {
      let mant1Word = m1[i]
      let mant1Lo = mant1Word & 0x7fff
      let mant1Hi = mant1Word >> 15

      let carry = 0
      for (let j = Math.min(tLen - i, m2Len - 1); j >= 0; --j) {
        let writeIndex = i + j

        let mant2Word = m2[j]
        let mant2Lo = mant2Word & 0x7fff
        let mant2Hi = mant2Word >> 15

        let low = Math.imul(mant1Lo, mant2Lo)
        let high = Math.imul(mant1Hi, mant2Hi)
        let middle = (Math.imul(mant1Hi, mant2Lo) + Math.imul(mant1Lo, mant2Hi)) | 0

        low +=
            ((middle & 0x7fff) << 15) +
            (writeIndex < tLen ? t[writeIndex] : 0) +
            carry
        low >>>= 0

        if (low > 0x3fffffff) {
          high += low >>> 30
          low &= 0x3fffffff
        }

        high += middle >> 15

        if (writeIndex < tLen) t[writeIndex] = low
        else ignoredLows += low // keep track of lows that we never actually included

        carry = high | 0
      }

      if (i > 0) {
        t[i - 1] += carry
      } else {
        highestWord = carry
      }
    }

    let shift = -1
    let trailingInfo = 0
    let maxErr = Math.ceil(ignoredLows / 0x40000000) + 2

    if (highestWord !== 0) {
      maxErr = Math.ceil((t[tLen - 1] + maxErr) / 0x40000000)
      rightShiftMantissa(t, 30)

      t[0] = highestWord
      shift = 0
    }

    let canBeRounded = canMantissaBeRounded(t, tLen, prec, tLen - 1, rm, 0, maxErr)

    if (!canBeRounded) {
      // TODO: make this more unlikely, probs by computing one extra word. Currently this fallback happens in about 10%
      //  of cases, which is way too much because it's super slow
      return slowExactMultiplyMantissas(m1, m1Len, m2, m2Len, t, tLen, prec, rm)
    }

    let roundingShift = roundMantissaToPrecision(
        t,
        tLen,
        t,
        tLen,
        prec,
        rm,
        trailingInfo
    )

    return shift + roundingShift
}

class BigFloat {
  /**
   * The sign of this float; can be ±0 (which are differentiated), ±1,  ±inf, and NaN. Special values are therefore signaled
   * by special sign values. If the sign is not ±1 (in other words, if it is a special sign) then the mantissa values
   * are exponent are arbitrary
   */
  sign: number
  /**
   * Exponent of this float; ranges between TODO
   */
  exp: number
  /**
   * Precision of this float; ranges between BIGFLOAT_MIN_PREC and BIGFLOAT_MAX_PREC
   */
  prec: number
  /**
   * Mantissa of this BigFloat, in 30-bit unsigned words
   */
  mant: Mantissa

  constructor (sign: number, exp: number, prec: number, mant: Mantissa) {
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
  static new (prec: number=WORKING_PRECISION): BigFloat {
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
  static fromNumber (n: number, prec: number=WORKING_PRECISION, rm: RoundingMode=WORKING_RM): BigFloat {
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
  toNumber (rm: RoundingMode = WORKING_RM, f32: boolean = false): number {
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
    let shift = roundMantissaToPrecision(m, mLen, roundedMantissa, 3, prec, rm as RoundingMode)

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
        shift = roundMantissaToPrecision(m, mLen, roundedMantissa, 3, exp - MIN_EXPONENT, rm as RoundingMode)
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
  setFromBigFloat (f: BigFloat, rm: RoundingMode=WORKING_RM): BigFloat {
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
  setFromNumber (n: number, rm: RoundingMode=WORKING_RM): BigFloat {
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
    let valExponent = getFloatStoreExponent()
    let valMantissa = getFloatStoreMantissa()

    // Exponent of the float (2^30)^newExp
    let newExp = Math.ceil((valExponent + 1) / BIGFLOAT_WORD_BITS)

    // The mantissa needs to be shifted to the right by this much. 0 < bitshift <= 30. If the number is denormal, we
    // have to shift it by one bit less
    let bitshift = newExp * BIGFLOAT_WORD_BITS - valExponent - (+nDenormal)
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

  /**
   * Add two floating point numbers, writing to the destination BigFloat
   * @param f1 {BigFloat}
   * @param f2 {BigFloat}
   * @param target {BigFloat}
   * @param rm {number} Rounding mode
   */
  static addTo (f1: BigFloat, f2: BigFloat, target: BigFloat, rm: RoundingMode=WORKING_RM) {
    let f1Sign = f1.sign, f2Sign = f2.sign
    if (!Number.isFinite(f1Sign) || !Number.isFinite(f2.sign)) {
      target.sign = f1Sign + f2Sign
      return
    }

    if (f1Sign === 0) {
      target.setFromBigFloat(f2, rm)
      return
    } else if (f2Sign === 0) {
      target.setFromBigFloat(f1, rm)
      return
    }

    let f1m = f1.mant, f2m = f2.mant, f1e = f1.exp, f2e = f2.exp
    let tm = target.mant, tml = tm.length, tPrec = target.prec

    if (f1Sign === f2Sign) {
      if (f1e < f2e) { // swap
        let tmp = f1m
        f1m = f2m
        f2m = tmp

        let tmp2 = f1e
        f1e = f2e
        f2e = tmp2
      }

      let f1ml = f1m.length, f2ml = f2m.length
      let shift = addMantissas(f1m, f1ml, f2m, f2ml, f1e - f2e, tm, tml, tPrec, rm)

      target.exp = shift + f1e
      target.sign = f1Sign
    }
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
export function toHex (a: number): string {
  return ((a < 0) ? '-' : '') + "0x" + leftZeroPad(Math.abs(a).toString(16), 8, '0')
}

export function toBinary (a: number): string {
  return ((a < 0) ? '-' : '') + "0b" + leftZeroPad(Math.abs(a).toString(2), 30, '0')
}

/**
 * Pretty print a mantissa for analysis
 * @param mantissa {Int32Array}
 * @param color {string} Optional, of the form \x1b[32m, etc; used for command-line prettifying
 * @param binary {string} Whether to display it as 30-bit padded binary
 * @returns {string}
 */
export function prettyPrintMantissa (mantissa: Mantissa, color: string="", binary: boolean=false): string {
  return '[ ' + Array.from(mantissa).map(binary ? toBinary : toHex)
    .map(s => `${color}${s}${color ? "\x1b[0m" : ''}`).join(', ') + ' ]'
}


export { BigFloat }
