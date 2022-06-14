
/**
 * Given a string like "1.5", "3e10", etc., determine whether it is an integer without evaluating it. Assumes the string
 * is well-formed.
 */
export function isStringInteger (s: string): boolean {
  if (s[0] === '-') s = s.slice(1) // trim leading '-'

  let exponent = 0, mIntTrailingZeros = Infinity, mFracLen = 0
  let e = s.indexOf('e')

  // If mFracLen = 0 (no fractional part) and mIntTrailingZeros = 0 (no integer part), the result is 0, so integer
  // If mFracLen > 0 (fractional part), integer if exponent >= mFracLen
  // If mFracLen = 0 (no fractional part), integer if exponent >= -mIntTrailingZeros

  if (e !== -1) { // get exponent
    exponent = parseInt(s.slice(e + 1), 10)

    if (Number.isNaN(exponent)) throw new Error("unrecognized exponent " + s.slice(e + 1))
  } else {
    e = s.length
  }

  let p = s.indexOf('.')
  if (p !== -1) {
    for (let i = e - 1; i > p; --i) {
      // find trailing zeros
      if (s[i] !== '0') {
        mFracLen = i - p
        break
      }
    }
  } else {
    p = e
  }

  for (let i = p - 1; i >= 0; --i) {
    if (s[i] !== '0') {
      mIntTrailingZeros = p - i
    }
  }

  if (mFracLen !== 0) {
    return exponent >= mFracLen
  } else {
    if (mIntTrailingZeros !== 0) {
      return exponent > -mIntTrailingZeros
    } else {
      // 0
      return true
    }
  }
}
