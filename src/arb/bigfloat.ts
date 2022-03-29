import {isRoundingMode, ROUNDING_MODE, RoundingMode} from "../other/rounding_modes.js"
import {leftZeroPad} from "../grapheme_shared.js"
import { flrLog2, getFloatStoreExponent, getFloatStoreMantissa, isDenormal, pow2, setFloatStore } from "../fp/manip.js"

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
 *              ^ index            gives 1.
 * mant = [ 0x3fffffff, 10000000, 00000001 ]
 *              ^ index            gives 3.
 * mant = [ 0x3fffffff, 10000000, 00000000 ]
 *              ^ index            gives 2.
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

  // Need to compute to higher precision first
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
export function subtractMantissas (m1: Mantissa, m1Len: number, m2: Mantissa, m2Len: number, m2shift: number, t: Mantissa, tLen: number, prec: number, rm: RoundingMode) {
  // My algorithm for (efficient) subtraction is a bit complicated. The reference implementation allocates a mantissa
  // large enough to store the exact result, computes the exact result, and rounds it. But if m2shift is large, this
  // approach allocates and calculates way more stuff than it needs to. Ideally, we allocate little or nothing and only
  // compute as much as necessary for the target mantissa to be rounded to prec bits.

  // We already assume that mant1 and mant2 are both valid mantissas, and that m2shift >= 0.

  m1Len |= 0
  m2Len |= 0
  tLen |= 0

  const MAX_SHIFT = BIGFLOAT_MAX_MANTISSA_LEN + 20

  // if m2shift is huge, just clamp it; no mantissa is long enough for it to matter
  m2shift = ((m2shift > MAX_SHIFT) ? MAX_SHIFT : m2shift) | 0

  let m2end = (m2Len + m2shift) | 0
  let exactEnd = m1Len > m2end ? m1Len : m2end // The end of exact computation, relative to the first word of mant1
  let exchanged = false // whether the mantissas have been exchanged

  // We can visualize the situation as follows:
  //  <--           mant1Len = 4                  -->
  // [ 0xcafecafe, 0xcafecafe, 0xcafecafe, 0xcafecafe]
  //  <-- mant2Shift = 2 --> [ 0xbeadbeef, 0xbeadbeef, 0xbeadbeef ]
  //                          <--         mant2Len = 3         -->
  //  <--               exactEnd = mant2End = 5                --> (maximum of mant1Len and mant2End)

  // We calculate words of the result relative to the first word of m1 (generally, this is how we index things). If a
  // word is 0, then the start of the result occurs later. If the first word we discover is negative, we exchange m1 and
  // m2, because m2 > m1. Then, if a word is 1, the start of the result may be there, or may be later, depending on the
  // next computed word: If the next computed word is negative, then the result begins later; if the next computed word
  // is 0, then the result may begin later; if the next computed word is positive, the result begins at the word that is
  // 1. If a word is 2 or greater, the start of the result is there.

  // mant1:      [ 0xcafecafe, 0xcafecafe, 0xcafecafd, 0x00000001 ]
  // mant2:      [ 0xcafecafe, 0xcafecafe, 0xcafecafe, 0x00000000 ]
  // computed words:    0           0           -1
  //                                            ^ need to exchange m1 and m2!
  // After exchanging...
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

  // mant1:      [ 0xcafecafe, 0xcafecafe, 0xcafecafe, 0x00000000 ]
  // mant2:      [ 0xcafecafe, 0xcafecafe, 0xcafecafd, 0x00000001 ]
  // computed words:    0           0           1 = positiveComputedWord
  //                                            ^ positiveComputedWordIndex

    // Before: [ 0x00000002, -0x3fffffff, -0x3fffffff, -0x3ffffffe ]
    // After:  [ 0x00000001,  0x00000000,  0x00000000,  0x00000002 ]
    //          n = 0 zero words

    // Before: [ 0x00000001, -0x3fffffff, -0x3fffffff, -0x3ffffffe ]
    // After:  [ 0x00000000,  0x00000000,  0x00000000,  0x00000002 ]
    //          <--       n = 3 zero words        -->

    // Before: [ 0x00000000,  0x00000000,  0x00000000,  0x00000002 ]
    //          <--       n = 3 zero words        -->
    // After:  [ 0x00000002,  0x00000000,  0x00000000,  0x00000000 ]
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
