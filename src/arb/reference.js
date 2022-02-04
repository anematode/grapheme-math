// Reference function. Slow but (hopefully) accurate
import {leftShiftMantissa, prettyPrintMantissa, rightShiftMantissa} from "./bigfloat.js"
import {ROUNDING_MODE} from "../rounding_modes.js"

const BIGFLOAT_WORD_BITS = 30
const RECIP_BIGFLOAT_WORD_BITS = 1 / BIGFLOAT_WORD_BITS
const BIGFLOAT_WORD_SIZE = 1 << BIGFLOAT_WORD_BITS
const BIGFLOAT_WORD_MAX = BIGFLOAT_WORD_SIZE - 1

const BIGFLOAT_MAX_EXP = Number.MAX_SAFE_INTEGER
const BIGFLOAT_MIN_EXP = -Number.MAX_SAFE_INTEGER

// Totally arbitrary, but whatever
const BIGFLOAT_MIN_PRECISION_BITS = 4
const BIGFLOAT_MAX_PRECISION_BITS = 16777216

const recip2Pow30 = 9.313225746154785e-10
const recip2Pow60 = 8.673617379884035e-19

// Slow, but hopefully accurate
export function addMantissas (mant1, mant1Len, mant2, mant2Len, mant2Shift, prec, target, targetLen, round=CURRENT_ROUNDING_MODE) {
  let output = new Int32Array(Math.max(mant1Len, mant2Len + mant2Shift) + 1)

  for (let i = 0; i < mant1Len; ++i) {
    output[i] += mant1[i]
  }

  for (let i = 0; i < mant2Len; ++i) {
    output[i + mant2Shift] += mant2[i]
  }

  let carry = 0
  for (let i = output.length - 1; i >= 0; --i) {
    let word = output[i] + carry

    if (word > 0x3fffffff) {
      word -= 0x40000000
      carry = 1
    } else {
      carry = 0
    }

    output[i] = word
  }

  if (carry === 1) {
    rightShiftMantissa(output, 30, output)
    output[0] = carry
  }

  let roundingShift = roundMantissaToPrecision(output, prec, target, round)

  return carry + roundingShift
}

export function subtractMantissas (mant1, mant2, mant2Shift, prec, target, round=CURRENT_ROUNDING_MODE) {
  let output = new Int32Array(Math.max(mant1.length, mant2.length + mant2Shift) + 1)

  for (let i = 0; i < mant1.length; ++i) {
    output[i] += mant1[i]
  }

  for (let i = 0; i < mant2.length; ++i) {
    output[i + mant2Shift] -= mant2[i]
  }

  let carry = 0
  for (let i = output.length - 1; i >= 0; --i) {
    let word = output[i] - carry

    if (word < 0) {
      word += 0x40000000
      carry = 1
    } else {
      carry = 0
    }

    output[i] = word
  }

  if (carry === 1) {
    throw new Error(`Invalid mantissas ${prettyPrintMantissa(mant1)}, ${prettyPrintMantissa(mant2)}`)
  }

  return roundMantissaToPrecision(output, prec, target, round)
}

export function multiplyMantissas (mant1, mant2, precision, targetMantissa, roundingMode) {
  let arr = new Int32Array(mant1.length + mant2.length + 1)

  for (let i = mant1.length; i >= 0; --i) {
    let mant1Word = mant1[i] | 0
    let mant1WordLo = mant1Word & 0x7fff
    let mant1WordHi = mant1Word >> 15

    let carry = 0,
      j = mant2.length - 1
    for (; j >= 0; --j) {
      let mant2Word = mant2[j] | 0
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
    leftShiftMantissa(arr, 30)
    shift -= 1
  }

  shift += roundMantissaToPrecision(
    arr,
    precision,
    targetMantissa,
    roundingMode
  )

  return shift
}

/**
 * Round an (unsigned) mantissa to a given precision, in one of a few rounding modes. Also returns a shift if the
 * rounding operation brings the float to a higher exponent. Trailing information may be provided about the digits
 * following the mantissa to ensure correct rounding in those cases. This function allows aliasing, meaning the target
 * mantissa and the given mantissa can be the same array, leading to an in-place operation
 * @param mant {Int32Array} Array of 30-bit mantissa words
 * @param prec {number} Precision, in bits, to round the mantissa to
 * @param target {Int32Array} The mantissa to write to
 * @param round {number} Rounding mode; the operation treats the number as positive
 * @param trailing {number} 0 if the mantissa is followed by infinite zeros; 1 if between 0 and 0.5; 2 if a tie; 3 if between a tie and 1
 * @param trailingMode {number} 0 if the trailingInfo is considered to be at the end of all the words; 1 if it's considered to be at the end of precision
 * @returns {number} The shift of the rounding operation; 1 or 0
 */
export function roundMantissaToPrecision (
  mant,
  prec,
  target,
  round = CURRENT_ROUNDING_MODE,
  trailing = 0,
  trailingMode = 0
) {
  let isAliased = mant === target
  let mantLen = mant.length

  if (round === ROUNDING_MODE.WHATEVER) {
    if (mant[0] === 0) {
      // Shifting needs to be done
      let shift = 0

      for (let i = 1; i < mantLen; ++i) {
        if (mant[i]) {
          shift = i
          break
        }
      }

      leftShiftMantissa(mant, shift * 30, target)
      return -shift
    }

    if (isAliased) return 0

    // Copy over the mantissa without rounding
    for (let i = target.length - 1; i >= 0; --i) {
      target[i] = mant[i]
    }

    return 0
  }

  let targetLen = target.length

  let offset = -1,
    shift = 0,
    bitShift = 0

  // How many ghost bits there are at the beginning; in other words, where to start counting precision bits from.
  // Specialized impl of clzMantissa
  for (let i = 0; i < mantLen; ++i) {
    if (mant[i]) {
      bitShift = 30 * i
      offset = bitShift + Math.clz32(mant[i]) - 2

      shift = -i | 0

      break
    }
  }

  if (offset === -1) {
    // Mantissa is all 0s, return
    for (let i = 0; i < targetLen; ++i) {
      target[i] = 0
    }

    return shift
  }

  // Copy over the given mantissa, shifted by shift
  leftShiftMantissa(mant, bitShift, target)
  offset -= bitShift

  // Which bit to start truncating at, indexing from 0 = the beginning of the mantissa
  let trunc = prec + offset
  let truncWord = (trunc / BIGFLOAT_WORD_BITS) | 0

  // Number of bits to truncate off the word, a number between 1 and 30 inclusive
  let truncateLen =
    BIGFLOAT_WORD_BITS - (trunc - truncWord * BIGFLOAT_WORD_BITS)

  // Remainder of the truncation and whether to do a carry after the truncation (rounding up)
  let rem = 0,
    doCarry = false

  // If the truncation would happen after the end of the mantissa...
  if (truncWord >= mantLen + shift) {
    // Whether the truncation bit is on the (nonexistent) word right after the mantissa
    let isAtVeryEnd =
      truncWord === mantLen + shift && truncateLen === BIGFLOAT_WORD_BITS

    // Fake a trailing info after the end. Our general strategy with trailingInfoMode = 1 is to convert it into a form
    // that trailingInfoMode = 0 can handle
    if (!isAtVeryEnd && trailingMode === 1 && trailing > 0) {
      // Any positive trailing info that isn't at the very end turns into a trailing info between 0 and 0.5 at the end
      trailing = 1
      isAtVeryEnd = true
    }

    // If rounding at the very end, what we do depends directly on the trailingInfo. To avoid complicating matters, we
    // "fake" the tie and round up cases so that the code doesn't have to be duplicated--especially the tie code, which
    // is slightly intricate
    if (isAtVeryEnd) {
      if (
        trailing === 0 ||
        round === ROUNDING_MODE.DOWN || round === ROUNDING_MODE.TOWARD_ZERO ||
        (trailing === 1 &&
          (round === ROUNDING_MODE.TIES_AWAY ||
            round === ROUNDING_MODE.TIES_EVEN))
      ) {
        return shift
      } else if (
        trailing === 2 &&
        (round === ROUNDING_MODE.TIES_AWAY || round === ROUNDING_MODE.TIES_EVEN)
      ) {
        rem = 0x20000000 // emulate tie = BIGFLOAT_WORD_SIZE / 2
      } else {
        rem = 0x30000000 // emulate round up = 3 * BIGFLOAT_WORD_SIZE / 4
      }
    } else {
      // Otherwise, if the rounding is happening after the very end, nothing happens since it's already all 0s
      return shift
    }
  } else {
    // Truncate the word
    let word = target[truncWord]
    let truncatedWord = (word >> truncateLen) << truncateLen
    target[truncWord] = truncatedWord

    // Store the remainder, aka what was just truncated off
    if (trailingMode === 0) {
      rem = word - truncatedWord
    } else {
      // When in info mode 1, we fake a remainder and trailing info that corresponds to the correct rounding mode.
      // 0 -> (0, 0), 1 (between 0 and 0.5) -> (0, positive), 2 -> (tie, 0), 3 -> (tie, (between 0 and 0.5))
      rem = trailing < 2 ? 0 : 1 << (truncateLen - 1)
      trailing &= 1
    }
  }

  // Determine whether to round up instead of truncating. Rounding up entails adding a 1 bit right where the mantissa
  // was truncated. For example, if we just truncated 011010110|1000, and our rounding mode is, say, TIES_AWAY, then we
  // determine that we have to round up and add 1 to the end: 01101011[1]. We call this a carry because it could
  // carry down the word in the right circumstances.
  doCarry: if (
    round === ROUNDING_MODE.UP ||
    round === ROUNDING_MODE.TOWARD_INF
  ) {
    // If we're rounding up, we carry if and only if the remainder is positive or there is a nonzero word after the
    // truncated word. If in info mode 1 we treat all the numbers following as 0 anyway, since that information is
    // contained within rem and trailingInfo
    if (rem > 0 || trailing > 0) {
      doCarry = true
    } else if (trailingMode === 0) {
      for (let i = truncWord - shift + 1; i < mantLen; ++i) {
        if (mant[i] !== 0) {
          doCarry = true
          break
        }
      }
    }
  } else if (
    round === ROUNDING_MODE.NEAREST ||
    round === ROUNDING_MODE.TIES_AWAY
  ) {
    // Truncated amounts less than this mean round down; more means round up; equals means needs to check whether the
    // rest of the limbs are 0, then break the tie
    let splitPoint = 1 << (truncateLen - 1)

    if (rem > splitPoint) {
      doCarry = true
    } else if (rem === splitPoint) {
      if (trailing > 0) {
        doCarry = true
      } else {
        if (trailingMode === 0) {
          // Try to break the tie by looking for nonzero bits
          for (let i = truncWord - shift + 1; i < mantLen; ++i) {
            if (mant[i] !== 0) {
              doCarry = true
              break doCarry
            }
          }
        }

        // Need to break the tie
        if (round === ROUNDING_MODE.TIES_EVEN) {
          // We only do the carry if it would give an even bit at the end. To do this we query for the bit which will be
          // affected (the truncateLen th bit). If the bit is 1, we do the carry. If truncateLen is 30 then we have to look
          // at the preceding word for the bit, since we truncated *at* a word

          let bit =
            truncateLen === BIGFLOAT_WORD_BITS
              ? target[truncWord - 1] & 1
              : (target[truncWord] >> truncateLen) & 1

          if (bit) doCarry = true
        } else {
          // Ties away from zero; always carry
          doCarry = true
        }
      }
    }
  }

  // Set all the words following the truncated word to 0
  for (let j = truncWord; ++j < targetLen;) {
    target[j] = 0
  }

  // The carry value is returned indicating whether the mantissa has "overflowed" due to rounding
  let carry = 0

  if (doCarry) {
    // Carry amount. Note that in the case of truncateLen = 30 we add 1 << 30 to a word, then immediately subtract
    // 2^30 and carry it to the next word, so everything works out
    carry = 1 << truncateLen

    for (let j = truncWord; j >= 0; --j) {
      let word = target[j] + carry

      if (word > BIGFLOAT_WORD_MAX) {
        word -= BIGFLOAT_WORD_SIZE
        target[j] = word
        carry = 1
      } else {
        target[j] = word
        carry = 0
        break // can immediately break
      }
    }
  }

  if (carry === 1) {
    // We carried the whole way and still have a 1, meaning the mantissa is now full of zeros and we need to shift by
    // one word and set the first word to a 1
    target[0] = 1

    return shift + 1
  }

  return shift
}
