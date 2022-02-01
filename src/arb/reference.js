// Reference function. Slow but (hopefully) accurate
import {leftShiftMantissa, prettyPrintMantissa, rightShiftMantissa, roundMantissaToPrecision} from "./bigfloat.js"

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
