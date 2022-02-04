import { ROUNDING_MODE } from "../rounding_modes.js"
import {leftZeroPad} from "../../grapheme_shared.js"

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

/**
 * The minimum number of words needed to store a mantissa with prec bits. The +2 is because the bits need to be stored
 * at any shift within the word, from 1 to 29, so some space may be needed.
 * @param prec {number}
 * @returns {number}
 */
export function neededWordsForPrecision (prec) {
  prec |= 0

  return ((prec - 1) / BIGFLOAT_WORD_BITS + 2) | 0
}

/**
 * Convert a number to a hex representation of exactly 8 digits
 * @param a {number}
 * @returns {string}
 */
export function toHex (a) {
  return ((a < 0) ? '-' : '') + "0x" + leftZeroPad(Math.abs(a).toString(16), 8, '0')
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

  let neededLen = neededWordsForPrecision(prec)
  if (mant?.length < neededLen) {
    throw new Error(`Float mantissa has length ${mant.length}, needs to have at least length ${neededLen} to handle precision ${prec}`)
  }

  if (Number.isFinite(sign) && sign !== 0 && sign !== 1 && sign !== -1) throw new RangeError(`Sign ${sign} is invalid`)
  if (exp > BIGFLOAT_MAX_EXP || exp < BIGFLOAT_MIN_EXP) throw new RangeError(`Exponent ${exp} is outside valid range [${BIGFLOAT_MIN_EXP}, ${BIGFLOAT_MAX_EXP}]`)

  if (!f.isSpecial()) {
    validateMantissa(f.mant)
  }
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
   */
  static new (prec) {
    if (prec < BIGFLOAT_MIN_PRECISION || prec > BIGFLOAT_MAX_PRECISION) {
      throw new RangeError(`Precision must be in range [${BIGFLOAT_MIN_PRECISION}, ${BIGFLOAT_MAX_PRECISION}]`)
    }

    // Empty mantissa
    let mant = new Int32Array(neededWordsForPrecision(prec))

    this.sign = 0
    this.exp = 0
    this.prec = prec
    this.mant = mant
  }

  /**
   * Whether this number is ±0, NaN, or ±inf and therefore is treated specially
   * @returns {boolean}
   */
  isSpecial () {
    return Number.isFinite(this.sign) || this.sign === 0
  }
}

export { BigFloat }
