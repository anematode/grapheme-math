(function (global, factory) {
    typeof exports === 'object' && typeof module !== 'undefined' ? factory(exports) :
    typeof define === 'function' && define.amd ? define(['exports'], factory) :
    (global = typeof globalThis !== 'undefined' ? globalThis : global || self, factory(global.Grapheme = {}));
}(this, (function (exports) { 'use strict';

    // Used for bit-level manipulation of floats
    const floatStore = new Float64Array(1);
    const intView = new Uint32Array(floatStore.buffer);
    const POSITIVE_NORMAL_MIN = 2.2250738585072014e-308;
    const NEGATIVE_NORMAL_MAX = -POSITIVE_NORMAL_MIN;
    const POSITIVE_DENORMAL_MIN = Number.MIN_VALUE;
    const MAX_VALUE = Number.MAX_VALUE;
    const MAGIC_ROUND_C = 1.1113332476497816e-16; // just above machine epsilon / 2
    /**
     * Returns the next floating point number after x. For example, roundUp(0) returns Number.MIN_VALUE.
     * inf -> inf, -inf -> -min negative value, nan -> nan, -0, 0 -> min positive denormal, max negative denormal -> 0. This
     * function is pretty darn fast and if it's inlined, is probably 2-4 ns / call.
     * @param x Any floating-point number
     * @returns The next representable floating-point number, handling special cases
     */
    function roundUp(x) {
        if (x >= -POSITIVE_NORMAL_MIN && x < POSITIVE_NORMAL_MIN) {
            // denormal numbers
            return x + POSITIVE_DENORMAL_MIN;
        }
        else if (x === -Infinity) {
            // special case
            return -MAX_VALUE;
        }
        return x + Math.abs(x) * MAGIC_ROUND_C;
    }
    /**
     * Returns the previous floating point number before x. Equivalent to -roundUp(-x)
     * @param x Any floating-point number
     * @returns The previous representable floating-point number, handling special cases
     */
    function roundDown(x) {
        if (x > -POSITIVE_NORMAL_MIN && x <= POSITIVE_NORMAL_MIN) {
            return x - POSITIVE_DENORMAL_MIN;
        }
        else if (x === Infinity) {
            return MAX_VALUE;
        }
        return x - Math.abs(x) * MAGIC_ROUND_C;
    }
    /**
     * Return whether a number is denormal; see {@link https://en.wikipedia.org/wiki/Denormal_number|Wikipedia} for a
     * technical explanation of what that means. ±0 are not considered denormal.
     * @param x Any floating-point number
     * @returns Whether the number is a denormal number
     */
    function isDenormal(x) {
        // Note that NaN will return false, since NaN < anything is false.
        return x !== 0 && x < POSITIVE_NORMAL_MIN && x > NEGATIVE_NORMAL_MAX;
    }
    /**
     * Get the non-biased exponent of a floating-point number x. Equivalent *mathematically* to floor(log2(abs(x))) for
     * finite values--but only if you're using infinite precision.
     * @param x Any floating-point number
     * @returns The non-biased exponent of that number's floating-point representation
     */
    function getExponent(x) {
        floatStore[0] = x;
        // Mask the biased exponent, retrieve it and convert it to non-biased
        return ((intView[1] & 0x7ff00000) >> 20) - 1023;
    }
    /**
     * Get the mantissa of a floating-point number as an integer in [0, 2^53).
     * @param x Any floating-point number
     * @returns An integer in [0, 2^53)
     */
    function getMantissa(x) {
        floatStore[0] = x;
        return intView[0] + _getMantissaHighWord() * 4294967296;
    }
    /**
     * Get the exponent and mantissa of a number
     * @param x
     */
    function getExponentAndMantissa(x) {
        floatStore[0] = x;
        return [
            getFloatStoreExponent(),
            getFloatStoreMantissa()
        ];
    }
    function _getMantissaHighWord() {
        return intView[1] & 0x000fffff;
    }
    /**
     * Set the internal float store value to be manipulated
     * @param x Any floating-point number
     */
    function setFloatStore(x) {
        floatStore[0] = x;
    }
    /**
     * Get the stored exponent after calling setFloatStore
     */
    function getFloatStoreExponent() {
        return ((intView[1] & 0x7ff00000) >> 20) - 1023;
    }
    /**
     * Get the stored mantissa after calling setFloatStore
     */
    function getFloatStoreMantissa() {
        return intView[0] + _getMantissaHighWord() * 4294967296;
    }
    /**
     * Testing function counting the *approximate* number of floats between x1 and x2, including x1 but excluding x2. NaN if
     * either is undefined. It is approximate because the answer may sometimes exceed Number.MAX_SAFE_INTEGER, but it is
     * exact if the answer is less than Number.MAX_SAFE_INTEGER.
     * @param x1 The lesser number
     * @param x2 The greater number
     * @returns The number of floats in the interval [x1, x2)
     */
    function countFloatsBetween(x1, x2) {
        if (Number.isNaN(x1) || Number.isNaN(x2)) {
            return NaN;
        }
        if (x1 === x2)
            return 0;
        if (x2 < x1) {
            const tmp = x1;
            x1 = x2;
            x2 = tmp;
        }
        const [x1man, x1exp] = frExp(x1);
        const [x2man, x2exp] = frExp(x2);
        return (x2man - x1man) * 2 ** 53 + (x2exp - x1exp) * 2 ** 52;
    }
    const pow2Lookup = new Float64Array(2098);
    let e = Number.MIN_VALUE;
    for (let i = -1074; i <= 1023; ++i) {
        pow2Lookup[i + 1074] = e;
        e *= 2;
    }
    /**
     * Calculates 2 ^ exp, using a customized method for integer exponents. An examination of fdlibm's pow function didn't
     * reveal any special handling, and indeed my benchmark indicates this method is 3 times faster than pow for integer
     * exponents. Note that bit shifts can't really be used except for a restricted range of exponents.
     * @param exp Exponent; intended for use with integers, but permits any floating-point number.
     * @returns Returns 2 ^ exp, and is guaranteed to be exact for integer exponents.
     */
    function pow2(exp) {
        if (!Number.isInteger(exp))
            return Math.pow(2, exp);
        if (exp > 1023)
            return Infinity;
        if (exp < -1074)
            return 0;
        exp |= 0;
        return pow2Lookup[exp + 1074];
    }
    // Counts the number of trailing zeros in a 32-bit integer n; similar to <i>Math.clz32</i>.
    function countTrailingZeros(n) {
        let bits = 0;
        if (n !== 0) {
            let x = n;
            // Suck off groups of 16 bits, then 8 bits, et cetera
            if ((x & 0x0000ffff) === 0) {
                bits += 16;
                x >>>= 16;
            }
            if ((x & 0x000000ff) === 0) {
                bits += 8;
                x >>>= 8;
            }
            if ((x & 0x0000000f) === 0) {
                bits += 4;
                x >>>= 4;
            }
            if ((x & 0x00000003) === 0) {
                bits += 2;
                x >>>= 2;
            }
            bits += (x & 1) ^ 1;
        }
        else {
            return 32;
        }
        return bits;
    }
    function _mantissaCtz() {
        const bits = countTrailingZeros(intView[0]);
        if (bits === 32) {
            const secondWordCount = countTrailingZeros(_getMantissaHighWord());
            return 32 + Math.min(secondWordCount, 20);
        }
        return bits;
    }
    /**
     * Counts the number of trailing zeros in the mantissa of a floating-point number, between 0 and 52.
     * @param d {number} A floating-point number
     * @returns {number} The number of trailing zeros in that number's mantissa
     */
    function mantissaCtz(d) {
        floatStore[0] = d;
        return _mantissaCtz();
    }
    function _mantissaClz() {
        const bits = Math.clz32(_getMantissaHighWord()) - 12; // subtract the exponent zeroed part
        return bits !== 20 ? bits : bits + Math.clz32(intView[0]);
    }
    /**
     * Counts the number of leading zeros in the mantissa of a floating-point number, between 0 and 52.
     * @param d A floating-point number
     * @returns The number of leading zeros in that number's mantissa
     */
    function mantissaClz(d) {
        floatStore[0] = d;
        return _mantissaClz();
    }
    /**
     * Converts a floating-point number into a fraction in [0.5, 1) or (-1, -0.5], except special cases, and an exponent,
     * such that fraction * 2 ^ exponent gives the original floating point number. If x is ±0, ±Infinity or NaN, [x, 0] is
     * returned to maintain this guarantee.
     * @param x Any floating-point number
     * @returns [fraction, exponent]
     */
    function frExp(x) {
        if (x === 0 || !Number.isFinite(x))
            return [x, 0];
        // +1 so that the fraction is between 0.5 and 1 instead of 1 and 2
        let exp = getExponent(x) + 1;
        // Denormal
        if (exp === -1022) {
            // If the mantissa is the integer m, then we should subtract clz(m) from exp to get a suitable answer
            exp -= _mantissaClz();
        }
        return [x / pow2(exp), exp];
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
    function rationalExp(x) {
        let [frac, denExponent, exp] = rationalExpInternal(x);
        let den = pow2(denExponent);
        return [frac * den, den, exp];
    }
    function rationalExpInternal(x) {
        if (x < 0) {
            const [num, den, exp] = rationalExpInternal(-x);
            return [-num, den, exp];
        }
        if (x === 0 || !Number.isFinite(x))
            return [x, 0, 0];
        // Decompose into frac * 2 ^ exp
        const [frac, exp] = frExp(x);
        // This tells us the smallest power of two which frac * (2 ** shift) is an integer, which is the denominator
        // of the dyadic rational corresponding to x
        const denExponent = 53 - mantissaCtz(frac);
        return [frac, denExponent, exp];
    }
    /**
     * Converts a floating-point number into an integer and exponent [i, e], so that i * 2^e gives the original number. i
     * will be within the bounds of Number.MAX_SAFE_INTEGER.
     * @param x Any floating-point number
     */
    function integerExp(x) {
        const [frac, denExponent, exp] = rationalExpInternal(x);
        return [frac * pow2(denExponent), exp - denExponent];
    }
    /**
     * Compute an accurate floor log 2 function. Note that Math.log2 is not good enough here;
     * floor(log2(268435455.99999994)), for example, returns 28 when the mathematical value is 27.
     * @param x Any floating-point number
     */
    function flrLog2(x) {
        let exp = getExponent(x) + 1;
        if (exp === -1022)
            exp -= _mantissaClz(); // denormal
        return exp - 1;
    }
    /**
     * Compute the unit in the last place for a given floating-point number. Returns NaN if the number is infinite and the
     * smallest positive denormal if it is equal to 0.
     * @param x Any floating-point number
     */
    function ulp(x) {
        if (!Number.isFinite(x))
            return Infinity;
        if (x === 0)
            return Number.MIN_VALUE;
        let exp = getExponent(x);
        if (exp === -1022)
            return Number.MIN_VALUE; // denormal
        return pow2(exp - 52);
    }

    const ROUNDING_MODE = {
        WHATEVER: 0,
        NEAREST: 0b10,
        TIES_EVEN: 0b10,
        TIES_ODD: 0b110,
        TIES_AWAY: 0b10010,
        TIES_ZERO: 0b10011,
        UP: 0b10000,
        DOWN: 0b10001,
        TOWARD_INF: 0b110000,
        TOWARD_ZERO: 0b110001 // toward zero
    };
    Object.freeze(ROUNDING_MODE);
    /**
     * Whether a given object is a recognized rounding mode
     * @param n Any object
     * @returns Whether it is a valid rounding mode
     */
    function isRoundingMode(n) {
        if (typeof n !== "number")
            return false;
        return (n === 0 || n === 0b10 || n === 0b110 || n === 0b10010 || n === 0b10011 || n === 0b10000 || n === 0b10001 || n === 0b110000 || n === 0b110001);
    }

    // File shared between all modules of Grapheme
    // Remember left-pad?
    function leftZeroPad(str, len, char = '0') {
        var _a;
        if (str.length >= len)
            return str;
        char = (_a = char[0]) !== null && _a !== void 0 ? _a : '0';
        return char.repeat(len - str.length) + str;
    }
    const warnings = new Map();
    function localWarn(s, id, maxCount = 2) {
        let count = warnings.get(id);
        if (count >= maxCount)
            return; // undefined casts to 0
        if (!count) {
            count = 0;
        }
        console.warn(`Warning ${id}: ${s}`);
        warnings.set(id, count + 1);
        if (count >= maxCount - 1) {
            console.warn(`Warning ${id} raised ${maxCount} times; no longer being reported`);
        }
    }

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
    const BIGFLOAT_WORD_BITS = 30;
    const BIGFLOAT_WORD_SIZE = 1 << BIGFLOAT_WORD_BITS;
    const BIGFLOAT_WORD_MAX = BIGFLOAT_WORD_SIZE - 1;
    const BIGFLOAT_MAX_EXP = Number.MAX_SAFE_INTEGER;
    const BIGFLOAT_MIN_EXP = -BIGFLOAT_MAX_EXP;
    const BIGFLOAT_MIN_PRECISION = 4; // in bits
    const BIGFLOAT_MAX_PRECISION = 1 << 24; // can be changed later
    const BIGFLOAT_MAX_MANTISSA_LEN = neededWordsForPrecision(BIGFLOAT_MAX_PRECISION); // Mantissas larger than this are impossible
    const recip2Pow30 = 9.313225746154785e-10; // 2^-30
    const recip2Pow60 = 8.673617379884035e-19; // 2^-60
    // Default precision and rounding mode values
    let WORKING_PRECISION = 53;
    let WORKING_RM = ROUNDING_MODE.NEAREST;
    /**
     * Set the working precision. Silently fails if the precision is invalid
     * @param p {number}
     */
    function setWorkingPrecision(p) {
        if (precisionInRange(p))
            WORKING_PRECISION = p;
    }
    /**
     * Get the working precision.
     * @returns {number}
     */
    function getWorkingPrecision() {
        return WORKING_PRECISION;
    }
    /**
     * Set the working rounding mode. Silently fails if the rounding mode is invalid
     * @param rm
     */
    function setWorkingRM(rm) {
        if (isRoundingMode(rm))
            WORKING_RM = rm;
    }
    /**
     * Get the working rounding mode
     */
    function getWorkingRM() {
        return WORKING_RM;
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
    function neededWordsForPrecision(prec) {
        prec |= 0;
        return ((prec - 1) / BIGFLOAT_WORD_BITS + 2) | 0;
    }
    // Whether a precision is valid
    function precisionInRange(prec) {
        return typeof prec === "number" && prec >= BIGFLOAT_MIN_PRECISION && prec <= BIGFLOAT_MAX_PRECISION;
    }
    // Throws if a precision is invalid
    function precisionInRangeThrows(prec) {
        if (typeof prec !== "number" || prec < BIGFLOAT_MIN_PRECISION || prec > BIGFLOAT_MAX_PRECISION)
            throw new RangeError(`Precision must be a number in range [${BIGFLOAT_MIN_PRECISION}, ${BIGFLOAT_MAX_PRECISION}], not ${prec}`);
    }
    function createMantissa(prec) {
        return new Int32Array(neededWordsForPrecision(prec));
    }
    /**
     * Throws if a mantissa is invalid, with a reason
     * @param m
     * @param mLen Length of the mantissa; defaults to the mantissa's total length
     */
    function validateMantissa(m, mLen = -1) {
        if (!(m instanceof Int32Array))
            throw new TypeError("Mantissa must be an Int32Array");
        if (mLen === -1)
            mLen = m.length;
        if (m.length === 0 || mLen > BIGFLOAT_MAX_MANTISSA_LEN)
            throw new RangeError(`Mantissa has length ${mLen}, must be between [1, ${BIGFLOAT_MAX_MANTISSA_LEN}]`);
        if (m[0] === 0)
            throw new Error(`First word of mantissa is zero`);
        for (let i = 0; i < mLen; ++i) {
            let word = m[i];
            if (word < 0 || word > BIGFLOAT_WORD_MAX)
                throw new RangeError(`Word at index ${i} is ${toHex(word)}, outside of valid range [0, ${BIGFLOAT_WORD_MAX}]`);
        }
        // ok
    }
    /**
     * Checks whether a BigFloat is valid and throws if not
     * @param f {BigFloat}
     */
    function validateBigFloat(f) {
        if (!(f instanceof BigFloat))
            throw new TypeError("f is not a BigFloat");
        let { sign, exp, prec, mant } = f;
        precisionInRangeThrows(prec);
        let neededLen = neededWordsForPrecision(prec);
        if ((mant === null || mant === void 0 ? void 0 : mant.length) < neededLen) {
            throw new Error(`Float mantissa has length ${mant.length}, needs to have at least length ${neededLen} to handle precision ${prec}`);
        }
        if (Number.isFinite(sign) && sign !== 0 && sign !== 1 && sign !== -1)
            throw new RangeError(`Sign ${sign} is invalid`);
        if (exp > BIGFLOAT_MAX_EXP || exp < BIGFLOAT_MIN_EXP)
            throw new RangeError(`Exponent ${exp} is outside valid range [${BIGFLOAT_MIN_EXP}, ${BIGFLOAT_MAX_EXP}]`);
        if (!f.isSpecial()) {
            validateMantissa(f.mant);
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
    function ulpError(m1, mLen, m2, m2shift, tLen, prec) {
        Math.clz32(m1[0]) - 2;
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
    function getTrailingInfo(mantissa, index) {
        let mantissaLen = mantissa.length;
        if (index >= 0) {
            if (index < mantissaLen) {
                if (mantissa[index] === 1 << 29) {
                    // Potential tie
                    for (let i = index + 1; i < mantissaLen; ++i) {
                        if (mantissa[i] !== 0)
                            return 3;
                    }
                    return 2;
                }
                else if (mantissa[index] > 1 << 29) {
                    return 3;
                }
            }
            else { // index < mantissaLen
                return 0;
            }
        }
        else { // index >= 0
            index = 0;
        }
        for (let i = index; i < mantissa.length; ++i) {
            if (mantissa[i] !== 0)
                return 1;
        }
        return 0;
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
    function roundMantissaToPrecision(m, mLen, t, tLen, prec, rm, trailing = 0) {
        mLen = mLen | 0;
        tLen = tLen | 0;
        prec = prec | 0;
        rm = (rm | 0);
        trailing = trailing | 0;
        if (rm === 0) {
            // rounding mode whatever
            if (m !== t) {
                // just need to copy over m to t (if there is no aliasing)
                let len = mLen > tLen ? tLen : mLen;
                for (let i = 0; i < len; ++i)
                    t[i] = m[i];
                // We don't even need to clear the rest of t to zeros, because it's rounding mode whatever
            }
            return 0;
        }
        let offset = Math.clz32(m[0]) - 2;
        let trunc = (prec + offset) | 0; // what BIT after which the mantissa should be all zeros
        let truncWordI = (trunc / BIGFLOAT_WORD_BITS) | 0; // which word this bit occurs in
        // How many bits needs to be removed off the word; always between 1 and 30 inclusive
        let truncLen = BIGFLOAT_WORD_BITS - (trunc - Math.imul(truncWordI, BIGFLOAT_WORD_BITS) /* will never overflow */);
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
            if (m !== t)
                for (let i = 0; i < mLen; ++i)
                    t[i] = m[i];
            // clear rest of target. the trailing info is not enough to recover any information
            for (let i = mLen; i < tLen; ++i)
                t[i] = 0;
            return 0;
        }
        // The word to be truncated, its truncated form, and how much will be removed
        let word = m[truncWordI];
        let truncWord = (word >> truncLen) << truncLen;
        let rem = word - truncWord;
        // true if we just truncate; false if we round up at the end of precision
        let doTruncation = true;
        // The rounding mode now matters
        if (rm & 2) { // ties
            let tieSplit = 1 << (truncLen - 1);
            if (rem < tieSplit) {
                doTruncation = true;
            }
            else if (rem > tieSplit) {
                doTruncation = false;
            }
            else {
                // Try to break the tie; any nonzero word implies a round up
                if (trailing >= 1)
                    doTruncation = false;
                else
                    for (let i = truncWordI + 1; i < mLen; ++i) {
                        if (m[i] > 0) {
                            doTruncation = false;
                            break;
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
                    if (rm === ROUNDING_MODE.TIES_AWAY)
                        doTruncation = false;
                    else if (rm === ROUNDING_MODE.TIES_ZERO)
                        doTruncation = true;
                    else {
                        // We examine the bit preceding the end of precision
                        let lastBit = truncLen === 30 ? (m[truncWordI - 1] & 1) : truncWord & (1 << truncLen);
                        if (lastBit)
                            doTruncation = rm === ROUNDING_MODE.TIES_ODD;
                        else
                            doTruncation = rm === ROUNDING_MODE.TIES_EVEN;
                    }
                }
            }
        }
        else if (!(rm & 1)) { // toward inf or up
            if (rem > 0)
                doTruncation = false;
            else {
                // any nonzero word means we don't truncate
                if (trailing)
                    doTruncation = false;
                else
                    for (let i = truncWordI + 1; i < mLen; ++i) {
                        if (m[i] > 0) {
                            doTruncation = false;
                            break;
                        }
                    }
            }
        }
        else {
            // Truncate no matter what
            doTruncation = true;
        }
        // Now that we know whether to truncate or carry up, we copy over the relevant words from the source to the target
        // mantissa and truncate or carry
        if (m !== t)
            for (let i = 0; i < truncWordI; ++i)
                t[i] = m[i];
        let shift = 0; // shift will be 1 if everything carries over
        if (!doTruncation) {
            // Carry up one bit
            truncWord = (truncWord + (1 << truncLen)) | 0;
            if (truncWord > BIGFLOAT_WORD_MAX) {
                // We need to do a carry!
                truncWord = 0;
                let i = truncWordI - 1;
                for (; i >= 0; --i) {
                    let word = t[i] + 1;
                    t[i] = word;
                    // We keep carrying until the word is not larger than 0x3fffffff
                    if (word > BIGFLOAT_WORD_MAX)
                        t[i] = 0;
                    else
                        break;
                }
                if (i === -1) {
                    // We carried all the way! The mantissa is now all zeros, but should be [ 0x00000001, 0 ... ] with a shift of 1
                    // (increment the exponent by 1)
                    t[0] = 1;
                    shift = 1;
                }
            }
        }
        t[truncWordI] = truncWord;
        for (let i = truncWordI + 1; i < tLen; ++i)
            t[i] = 0; // clear the remainder of target
        return shift;
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
    function addMantissas(m1, m1l, m2, m2l, m2shift, t, tLen, prec, rm) {
        let isAliased = m1 === t;
        let mant2End = m2l + m2shift;
        let newMantLen = tLen;
        let newMant = t;
        // Need to compute to higher precision first
        if (m1l > newMantLen) {
            let neededWords = neededWordsForPrecision(prec);
            newMantLen = m1l > neededWords ? m1l : neededWords;
            newMant = new Int32Array(newMantLen);
        }
        // We first copy over all the parts of the addition we definitely need:
        if (!isAliased) {
            for (let i = 0; i < m1l; ++i) {
                newMant[i] = m1[i];
            }
            for (let i = m1l; i < newMantLen; ++i) {
                newMant[i] = 0;
            }
        }
        let mant2Bound1 = mant2End < newMantLen ? mant2End : newMantLen;
        for (let i = m2shift; i < mant2Bound1; ++i) {
            newMant[i] += m2[i - m2shift];
        }
        // Do the carry
        let carry = 0;
        for (let i = m1l - 1; i >= 0; --i) {
            let word = newMant[i] + carry;
            if (word > 0x3fffffff) {
                word -= 0x40000000;
                newMant[i] = word;
                carry = 1;
            }
            else {
                newMant[i] = word;
                carry = 0;
            }
        }
        // All that remains are the words of mant2 to the right of newMantLen - mant2Shift
        let trailingInfo = 0;
        let needsTrailingInfo = (rm & 2) || (!(rm & 1)); // ties or inf/up
        if (needsTrailingInfo) {
            let trailingShift = newMantLen - m2shift;
            trailingInfo = getTrailingInfo(m2, trailingShift > 0 ? trailingShift : 0);
            // If the trailing info is shifted, then round it to 0 or 1 as appropriate
            if (trailingShift < 0)
                trailingInfo = +!!trailingInfo;
        }
        let shift = 0;
        if (carry) {
            // Get trailing info from beyond the end of the truncation due to right shifting LOL
            if (needsTrailingInfo) {
                let lastWord = newMant[newMantLen - 1];
                if (lastWord === 0) {
                    trailingInfo = +!!trailingInfo;
                }
                else if (lastWord < 0x20000000) {
                    trailingInfo = 1;
                }
                else if (lastWord === 0x20000000) {
                    trailingInfo = trailingInfo ? 3 : 2;
                }
                else {
                    trailingInfo = 3;
                }
            }
            for (let i = newMantLen - 2; i >= 0; --i)
                newMant[i + 1] = newMant[i];
            newMant[0] = 1;
            shift += 1;
        }
        let roundingShift = rm === ROUNDING_MODE.WHATEVER ? 0 : roundMantissaToPrecision(newMant, newMantLen, t, tLen, prec, rm, trailingInfo);
        return roundingShift + shift;
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
    function subtractMantissas(m1, m1Len, m2, m2Len, m2shift, t, tLen, prec, rm) {
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
        constructor(sign, exp, prec, mant) {
            this.sign = sign;
            this.exp = exp;
            this.prec = prec;
            this.mant = mant;
        }
        /**
         * Create a BigFloat, initialized to zero, of a given precision
         * @param prec {number}
         * @returns {BigFloat}
         */
        static new(prec = WORKING_PRECISION) {
            precisionInRangeThrows(prec);
            let mant = createMantissa(prec);
            return new BigFloat(0, 0, prec, mant);
        }
        /**
         * Create a BigFloat from a JS number, rounding in the given direction. Special numbers will be preserved
         * @param n {number} Any JS number
         * @param prec {number} Precision
         * @param rm {number} Rounding mode
         * @returns {BigFloat}
         */
        static fromNumber(n, prec = WORKING_PRECISION, rm = WORKING_RM) {
            let f = BigFloat.new(prec);
            f.setFromNumber(n, rm);
            return f;
        }
        /**
         * Convert this BigFloat to a normal JS number, rounding in the given direction and optionally rounding to the nearest
         * float32 value. That functionality is more given to verify the double logic. It *does* handle denormal numbers,
         * unfortunately for me.
         * @param rm {number}
         * @param f32 {boolean} Whether to cast to a float32 instead of a float64
         * @returns {number}
         */
        toNumber(rm = WORKING_RM, f32 = false) {
            if (this.isSpecial())
                return this.sign;
            let m = this.mant, unshiftedExp = (this.exp - 1) * BIGFLOAT_WORD_BITS, mLen = m.length; // exp in base 2
            if (!rm) {
                // Rounding mode whatever: Short-circuit calculation for efficiency
                let m0 = m[0], m1 = m[1], m2 = (mLen < 3) ? 0 : m[2];
                return pow2(unshiftedExp) * (m0 + m1 * recip2Pow30 + m2 * recip2Pow60);
            }
            let prec = f32 ? 24 : 53;
            let roundedMantissa = SCRATCH_MANTISSA;
            if (rm & 16 && this.sign === -1)
                rm ^= 1; // flip rounding mode for sign
            // Round to the nearest float32 or float64, ignoring denormal numbers for now
            let shift = roundMantissaToPrecision(m, mLen, roundedMantissa, 3, prec, rm);
            let MIN_EXPONENT = f32 ? -149 : -1074;
            let MIN_NORMAL_EXPONENT = f32 ? -126 : -1022;
            let MAX_EXPONENT = f32 ? 127 : 1023;
            let MIN_VALUE = f32 ? 1.175494e-38 : Number.MIN_VALUE;
            let MAX_VALUE = f32 ? 3.40282347e38 : Number.MAX_VALUE;
            // Calculate an exponent and mant such that mant * 2^exponent = the number
            let mAsInt = 0, expShift = 0, exp = 0, denormal = false;
            do {
                if (shift) {
                    mAsInt = 1 << 30;
                }
                else {
                    mAsInt =
                        roundedMantissa[0] +
                            roundedMantissa[1] * recip2Pow30 +
                            (f32 ? 0 : roundedMantissa[2] * recip2Pow60); // because the mantissa is rounded, this is exact
                }
                // Normalize mant to be in the range [0.5, 1)
                expShift = flrLog2(mAsInt) + 1;
                mAsInt /= pow2(expShift);
                exp = unshiftedExp + expShift;
                if (exp < MIN_NORMAL_EXPONENT && exp >= MIN_EXPONENT && !denormal) {
                    // denormal, round to a different precision
                    shift = roundMantissaToPrecision(m, mLen, roundedMantissa, 3, exp - MIN_EXPONENT, rm);
                    denormal = true; // go back and calculate mAsInt
                }
                else
                    break;
            } while (denormal);
            // If the exponent is outside of bounds, we clamp it to a value depending on the rounding mode
            if (exp < MIN_EXPONENT) {
                if (rm & 2) { // tie
                    // Deciding between 0 and MIN_VALUE. Unfortunately at 0.5 * 2^-1074 there is a TIE omg
                    if (exp === MIN_EXPONENT - 1) {
                        // If greater or ties away
                        if (mAsInt > 0.5 || (rm === ROUNDING_MODE.TIES_AWAY
                            || rm === ROUNDING_MODE.TIES_ODD)) {
                            return this.sign * MIN_VALUE;
                        }
                    }
                    return this.sign * 0;
                }
                else {
                    if (!(rm & 1)) { // ties up or inf; note that rm has been flipped already to correspond to the magnitude
                        return this.sign * MIN_VALUE;
                    }
                    else { // down
                        return this.sign * 0;
                    }
                }
            }
            else if (exp > MAX_EXPONENT) {
                if (exp === MAX_EXPONENT + 1) {
                    // Bottom formula will overflow, so we adjust
                    return this.sign * mAsInt * 2 * pow2(exp - 1);
                }
                // 2^(MAX_EXPONENT + 1) ties between MAX_VALUE and Infinity. Ties to even, toward inf gives the latter; ties to
                // odd, toward zero gives the former
                if (exp === MAX_EXPONENT + 2 && mAsInt === 0.5) {
                    if (rm === ROUNDING_MODE.TIES_EVEN || rm === ROUNDING_MODE.TIES_AWAY)
                        return this.sign * Infinity;
                    else
                        return this.sign * MAX_VALUE;
                }
                if (rm & 1) {
                    // ties down in magnitude or toward zero
                    return this.sign * MAX_VALUE;
                }
                else {
                    return this.sign * Infinity;
                }
            }
            else {
                return this.sign * mAsInt * pow2(exp);
            }
        }
        /**
         * Set the value of this BigFloat from a BigFloat, keeping this float's precision.
         * @param f
         * @param rm
         * @returns {BigFloat} This, for chaining
         */
        setFromBigFloat(f, rm = WORKING_RM) {
            if (!(f instanceof BigFloat))
                throw new TypeError("BigFloat.setFromBigFloat takes a BigFloat");
            let fs = f.sign;
            this.sign = fs;
            if (fs === 0 || !Number.isFinite(fs))
                return this;
            let fm = f.mant, fml = fm.length, fe = f.exp, tm = this.mant, tml = tm.length;
            let shift = roundMantissaToPrecision(fm, fml, tm, tml, this.prec, rm); // copy over
            let e = shift + fe;
            if (e > BIGFLOAT_MAX_EXP) {
                // overflow
                this.sign = Infinity;
            }
            else {
                this.exp = e;
            }
            return this;
        }
        /**
         * Set the value of this BigFloat from a JS number. TODO: make more efficient
         * @param n {number}
         * @param rm {number} Rounding mode to be used; only relevant if prec < 53
         * @returns {BigFloat}
         */
        setFromNumber(n, rm = WORKING_RM) {
            if (this.prec < 53) {
                // Weird and rare case. Rounding to a lower precision is needed
                SCRATCH_DOUBLE.setFromNumber(n);
                this.setFromBigFloat(SCRATCH_DOUBLE);
                return this;
            }
            if (typeof n !== "number")
                throw new TypeError("BigFloat.setFromNumber takes a JS number");
            n = +n;
            const mant = this.mant; // mant.length guaranteed >= 3
            let nDenormal = isDenormal(n);
            setFloatStore(n);
            let valExponent = getFloatStoreExponent();
            let valMantissa = getFloatStoreMantissa();
            // Exponent of the float (2^30)^newExp
            let newExp = Math.ceil((valExponent + 1) / BIGFLOAT_WORD_BITS);
            // The mantissa needs to be shifted to the right by this much. 0 < bitshift <= 30. If the number is denormal, we
            // have to shift it by one bit less
            let bitshift = newExp * BIGFLOAT_WORD_BITS - valExponent - (+nDenormal);
            let denom = pow2(bitshift + 22);
            mant[0] =
                Math.floor(valMantissa / denom) /* from double */ +
                    (nDenormal ? 0 : 1 << (30 - bitshift)); /* add 1 if not denormal */
            let rem = valMantissa % denom;
            if (bitshift > 8) {
                let cow = 1 << (bitshift - 8);
                mant[1] = Math.floor(rem / cow);
                mant[2] = rem % cow << (38 - bitshift);
            }
            else {
                mant[1] = rem << (8 - bitshift);
                mant[2] = 0;
            }
            // Special handling; for tiny denormal numbers, the first word is 0, so we shift them over
            if (nDenormal && mant[0] === 0) {
                mant[0] = mant[1];
                mant[1] = mant[2];
                mant[2] = 0;
                newExp -= 1;
            }
            // Clear mantissa
            for (let i = mant.length - 1; i >= 3; --i)
                mant[i] = 0;
            this.exp = newExp;
            this.sign = Math.sign(n);
            return this;
        }
        /**
         * Whether this number is ±0, NaN, or ±inf and therefore is treated specially
         * @returns {boolean}
         */
        isSpecial() {
            return !Number.isFinite(this.sign) || this.sign === 0;
        }
        /**
         * Add two floating point numbers, writing to the destination BigFloat
         * @param f1 {BigFloat}
         * @param f2 {BigFloat}
         * @param target {BigFloat}
         * @param rm {number} Rounding mode
         */
        static addTo(f1, f2, target, rm = WORKING_RM) {
            let f1Sign = f1.sign, f2Sign = f2.sign;
            if (!Number.isFinite(f1Sign) || !Number.isFinite(f2.sign)) {
                target.sign = f1Sign + f2Sign;
                return;
            }
            if (f1Sign === 0) {
                target.setFromBigFloat(f2, rm);
                return;
            }
            else if (f2Sign === 0) {
                target.setFromBigFloat(f1, rm);
                return;
            }
            let f1m = f1.mant, f2m = f2.mant, f1e = f1.exp, f2e = f2.exp;
            let tm = target.mant, tml = tm.length, tPrec = target.prec;
            if (f1Sign === f2Sign) {
                if (f1e < f2e) { // swap
                    let tmp = f1m;
                    f1m = f2m;
                    f2m = tmp;
                    let tmp2 = f1e;
                    f1e = f2e;
                    f2e = tmp2;
                }
                let f1ml = f1m.length, f2ml = f2m.length;
                let shift = addMantissas(f1m, f1ml, f2m, f2ml, f1e - f2e, tm, tml, tPrec, rm);
                target.exp = shift + f1e;
                target.sign = f1Sign;
            }
        }
    }
    const SCRATCH_MANTISSA = createMantissa(53);
    const SCRATCH_DOUBLE = BigFloat.new(53);
    // Convenience functions
    /**
     * Convert a number to a hex representation of exactly 8 digits
     * @param a {number}
     * @returns {string}
     */
    function toHex(a) {
        return ((a < 0) ? '-' : '') + "0x" + leftZeroPad(Math.abs(a).toString(16), 8, '0');
    }
    function toBinary(a) {
        return ((a < 0) ? '-' : '') + "0b" + leftZeroPad(Math.abs(a).toString(2), 30, '0');
    }
    /**
     * Pretty print a mantissa for analysis
     * @param mantissa {Int32Array}
     * @param color {string} Optional, of the form \x1b[32m, etc; used for command-line prettifying
     * @param binary {string} Whether to display it as 30-bit padded binary
     * @returns {string}
     */
    function prettyPrintMantissa(mantissa, color = "", binary = false) {
        return '[ ' + Array.from(mantissa).map(binary ? toBinary : toHex)
            .map(s => `${color}${s}${color ? "\x1b[0m" : ''}`).join(', ') + ' ]';
    }

    // Abstraction of a concrete type--an actual class of JS object or primitive used in calculations. For example, a bool
    // and a number are both primitives, while a Grapheme.FastRealInterval is an object. Both are concrete types, although
    // a number and FastRealinterval may both represent the abstract type "real". (See below)
    class ConcreteType {
        constructor(params) {
            var _a, _b, _c, _d, _e, _f, _g;
            let name = this.name = params.name;
            // Whether the type is a primitive, and thus whether it can be "written to"
            this.isPrimitive = !!params.isPrimitive;
            // Function which, when called, returns a new instance of the type
            if (!params.init) {
                throw new TypeError(`ConcreteType ${name} needs an initializer`);
            }
            this.init = params.init;
            if (!params.castPermissive) {
                throw new TypeError(`ConcreteType ${name} needs a permissive casting function`);
            }
            // FUNCTION which, when called with a single argument, attempts to construct the type and will never fail (will
            // returned the undefined version instead)
            this.castPermissive = (_a = params.castPermissive) !== null && _a !== void 0 ? _a : null;
            // Default value
            this.defaultValue = this.init();
            // STRING which, when eval-ed, returns a new instance of the type (used for primitives only)
            this.initStr = (_b = params.initStr) !== null && _b !== void 0 ? _b : null;
            // Returns true if the passed parameter is of this type
            this.typecheck = (_c = params.typecheck) !== null && _c !== void 0 ? _c : null;
            // Returns a verbose error message if the passed parameter is not of this type, otherwise an empty string
            this.typecheckVerbose = (_d = params.typecheckVerbose) !== null && _d !== void 0 ? _d : null;
            // Returns true if the passed parameter is considered defined. For example, Complex(0, NaN) would give false
            this.isDefined = (_e = params.isDefined) !== null && _e !== void 0 ? _e : null;
            // FUNCTION which, when called with a single argument, deep clones the type. Only used for non-primitives
            this.clone = (_f = params.clone) !== null && _f !== void 0 ? _f : (x => x);
            // FUNCTION which, when called with two arguments src and dst, deep copies the contents of src to dst. Only used
            // for non-primitives
            this.copyTo = (_g = params.copyTo) !== null && _g !== void 0 ? _g : null;
            this.fillDefaults();
        }
        // Convenience method to avoid duplicate code for primitives
        fillDefaults() {
            if (this.isPrimitive) {
                if (!this.init || !this.initStr) {
                    let init, initStr;
                    if (typeof this.defaultValue === "number") { // nullable booleans, ints, and reals are all represented by a JS number
                        init = () => 0;
                        initStr = "0";
                    }
                    else {
                        throw new Error("Invalid primitive");
                    }
                    this.init = init;
                    this.initStr = initStr;
                }
            }
        }
        isSameConcreteType(concreteType) {
            return this.name === concreteType.name;
        }
        toHashStr() {
            return this.name;
        }
        prettyPrint() {
            return this.name;
        }
    }
    // Abstraction of a type. An expression or subexpression has a type. In the future we might want generics, so we use a
    // class instead of a string. Note the distinction between this "type" and the concrete types used underneath. For
    // example, "complex" is a type, while the Grapheme.Complex class is a concrete type. The unqualified word "type" refers
    // to the former; "concrete type" is used in the latter case.
    class MathematicalType {
        constructor(params) {
            // Name of the type
            this.name = params.name;
        }
        isSameType(type) {
            return this.name === type.name;
        }
        // Used for dictionary lookups (may change l8r)
        toHashStr() {
            return this.name;
        }
        prettyPrint() {
            return this.name;
        }
    }

    /**
     * Grapheme uses nullable booleans sometimes, to signify that the result of a boolean operation is undefined. For
     * example, sqrt(-1) == sqrt(-2) and sqrt(-1) == sqrt(-1) will both return the null boolean. We represent booleans as
     * 0 (false), 1 (true), or NaN (null). The reasoning here is that we want to propagate undefinedness. If an undefined
     * operation like sqrt(-1) is hidden away behind some boolean operation that in turn returns false, the result may
     * appear "defined" when it should be considered meaningless, since an invalid operation was used. For example, suppose
     * piecewise(condition, a, b) is a when condition is true and b when condition is false. For example, piecewise(x > 0, 1,
     * 0) is 1 for positive x and 0 for nonpositive x. But what if there was something like piecewise(sqrt(x) < 1, 1, 0)?
     * Then it is clearly 1 for x in [0, 1) and 0 for x in [1, inf), but what about x < 0? The sqrt operation is undefined
     * in that range. We have a couple options: throw an error, pretend that undefined operations are false, or return
     * an undefined value (in this case, probably NaN). The last case is the most sensible, but then we need some third
     * boolean value besides true or false. Thus, we use the nullable boolean.
     *
     * Ints and reals have their own "nullable" values: NaN. Interval classes usually have some form of definedness built
     * into them. The only tricky problem is the boolean. We *could* use (true, false, undefined) or (true, false, NaN) to be
     * the representation, but these use different underlying primitives. Choosing (0, 1, NaN) means everything operates in
     * floating-point, enabling greater optimization. Furthermore, NaN is implicitly converted to false in a boolean
     * expression, so user code that doesn't handle undefinedness will treat undefined results as false--which is
     * usually what is intended.
     *
     * TODO: Optimize. Math.sign is a slow builtin
     */
    // All these functions assume their inputs are in (-0, +0, 1, false, true, NaN). They return results in the range (0, 1, NaN)
    // and propagate NaNs as appropriate
    const Functions = Object.freeze({
        not: (a) => 1 - a,
        and: (a, b) => a * b,
        or: (a, b) => Math.sign(a + b),
        eq: (a, b) => 1 - Math.abs(a - b),
        notEq: (a, b) => Math.abs(a - b)
    });
    // Compare numerical values, taking into account NaNs and returning nullable booleans. Infinities are treated
    // differently than normal; -inf < inf is true and -inf > inf is false, but inf < inf is NaN, for example.
    const Comparisons = Object.freeze({
        less: (a, b) => 1 - Math.sign(Math.sign(a - b) + 1),
        lessEq: (a, b) => 0 - Math.sign(Math.sign(a - b) - 1),
        greater: (a, b) => 1 - Math.sign(Math.sign(b - a) - 1),
        greaterEq: (a, b) => 0 - Math.sign(Math.sign(b - a) - 1),
        eq: (a, b) => 1 - Math.sign(Math.abs(a - b)),
        notEq: (a, b) => Math.sign(Math.abs(a - b))
    });
    // Test for values
    const Test = Object.freeze({
        true: (a) => a === 1,
        false: (a) => a === 0,
        defined: (a) => a === a,
        undefined: (a) => a !== a
    });
    /**
     * Convert any object to a nullable boolean. Anything falsy, except NaN, is converted to 0. NaN is converted to NaN, and
     * everything else is converted to 1.
     *
     * @param b Any object
     * @returns The nullable boolean
     */
    function toNullableBoolean(b) {
        if (b == null || b !== b)
            return NaN;
        return +!!b;
    }
    /**
     * Returns true if b can be used as if it were a nullable boolean (i.e., it is one of -0, 0, false, true, or NaN)
     * because of implicit conversions
     * @param b Any object
     * @returns Whether b can be used a a nullable boolean
     */
    function isUsableNullableBoolean(b) {
        return b === 0 || b !== b || typeof b === "boolean";
    }
    /**
     * Returns true if b is strictly a nullable boolean (i.e., it is one of 0, 1, or NaN)
     * @param b Any object
     * @returns Whether b is a nullable boolean
     */
    function isNullableBoolean(b) {
        return b === 1 || b !== b || Object.is(b, 0);
    }
    /**
     * Returns a descriptive nonempty string if b is not a usable nullable boolean
     * @param b Any object
     * @returns Error string if argument is not a usable nullable boolean; empty otherwise
     */
    function typecheckUsableNullableBoolean(b) {
        if (typeof b === "boolean")
            return "";
        if (typeof b === "number") {
            if (b === 0 || b === 1 || b !== b)
                return "";
            return `Expected nullable boolean (±0, 1, NaN, false, or true), got number ${b}`;
        }
        return `Expected nullable boolean (±0, 1, NaN, false, or true), got type ${typeof b}`;
    }
    const NullableBoolean = Object.freeze({
        Functions,
        Comparisons,
        Test,
        toNullableBoolean,
        isUsableNullableBoolean,
        isNullableBoolean,
        typecheckUsableNullableBoolean
    });

    class FastBooleanInterval {
        constructor(min, max, info) {
            this.min = !!min;
            this.max = !!max;
            this.info = info | 0;
        }
    }

    /**
     * Integers are interesting, in that ideally we want to catch overflow (ex., 3^100 will not be represented exactly), but
     * in many contexts that overflow isn't what we care about, and we just want float values. For now, we will let integers
     * be anything except non-integer finite values. In other words, we permit integers to be -Infinity, Infinity, NaN, and
     * any integer, but not something like 0.5. We include -0.
     */
    const builtinIsInteger = Number.isInteger;
    const builtinIsFinite = Number.isFinite;
    /**
     * Check if a parameter is a valid nullable integer (must be an integer number or NaN, ±Infinity)
     * @param i Anything
     * @returns Whether the given parameter is a valid nullable integer
     */
    function isNullableInteger(i) {
        return builtinIsInteger(i) || (typeof i === "number" && !builtinIsFinite(i));
    }
    /**
     * Return a descriptive error message if a number is not a valid nullable integer; otherwise, return the empty string
     * @param i Anything
     * @returns Error message if the parameter is an invalid nullable integer; otherwise, an empty message
     */
    function typecheckNullableInteger(i) {
        let isInteger = builtinIsInteger(i);
        if (isInteger)
            return "";
        if (typeof i !== "number")
            return "Expected nullable integer (integer, ±Infinity, or NaN), found type " + (typeof i);
        let isFinite = builtinIsFinite(i);
        if (!isFinite)
            return "Expected nullable integer, found non-integral number " + i;
        return "";
    }
    const NullableInteger = Object.freeze({
        isNullableInteger,
        typecheckNullableInteger
    });

    /**
     * A real interval with only min, max, defMin (bit 0), defMax (bit 1), contMin (bit 2), contMax (bit 3)
     * TODO: types, functions
     */
    class FastRealInterval {
        constructor(min = 0, max = min, info = 0b111) {
            this.min = +min;
            this.max = +max;
            this.info = info | 0;
        }
        defMin() {
            return this.info & 0b1;
        }
        defMax() {
            return this.info & 0b10;
        }
        static fromObj(o) {
            let min = 0, max = 0, info = 0b111, isStr = false;
            if (typeof o === "string") {
                isStr = true;
                o = +o;
            }
            if (typeof o === "number") {
                if (Number.isNaN(o)) {
                    info = 0;
                }
                else {
                    min = max = o;
                    if (isStr) { // safely include the number
                        min = roundDown(min);
                        max = roundUp(max);
                    }
                }
            }
            else {
                throw "unimplemented";
            }
            return new FastRealInterval(min, max, info);
        }
        cont() {
            return this.info & 0b100;
        }
        static set(src, dst) {
            dst.min = src.min;
            dst.max = src.max;
            dst.info = src.info;
        }
        static setNumber(num, dst) {
            if (Number.isNaN(num)) {
                dst.info = 0;
            }
            else {
                dst.min = num;
                dst.max = num;
                dst.info = 0b111;
            }
        }
        static setRange(min, max, dst) {
            dst.min = min;
            dst.max = max;
            dst.info = 0b111;
        }
        /**
         * Add two fast real intervals, sending the result to dst
         * @param src1 {FastRealInterval}
         * @param src2 {FastRealInterval}
         * @param dst {FastRealInterval}
         * @param correctRounding {boolean} Whether to use correct rounding so the result is mathematically guaranteed
         */
        static add(src1, src2, dst, correctRounding) {
            let info = src1.info & src2.info;
            if (info === 0) {
                dst.info = 0;
                return;
            }
            let min = src1.min + src2.min;
            let max = src1.max + src2.max;
            if (correctRounding) {
                min = roundDown(min);
                max = roundUp(max);
            }
            dst.min = min;
            dst.max = max;
            dst.info = info;
        }
        /**
         * Subtract two fast real intervals, sending the result to dst
         * @param src1 {FastRealInterval}
         * @param src2 {FastRealInterval}
         * @param dst {FastRealInterval}
         * @param correctRounding {boolean} Whether to use correct rounding so the result is mathematically guaranteed
         */
        static sub(src1, src2, dst, correctRounding) {
            let info = src1.info & src2.info;
            if (info === 0) {
                dst.info = 0;
                return;
            }
            let min = src1.min - src2.max;
            let max = src1.max - src2.min;
            if (correctRounding) {
                min = roundDown(min);
                max = roundUp(max);
            }
            dst.min = min;
            dst.max = max;
            dst.info = info;
        }
        /**
         * Negate a real interval, sending the result to dst
         * @param src {FastRealInterval}
         * @param dst {FastRealInterval}
         * @param correctRounding {boolean} Whether to use correct rounding so the result is mathematically guaranteed
         */
        static unarySub(src, dst, correctRounding) {
            dst.min = -src.max;
            dst.max = -src.min;
            dst.info = src.info;
        }
        /**
         * Multiply two fast real intervals, sending the result to dst
         * @param src1 {FastRealInterval}
         * @param src2 {FastRealInterval}
         * @param dst {FastRealInterval}
         * @param correctRounding {boolean} Whether to use correct rounding so the result is mathematically guaranteed
         */
        static mul(src1, src2, dst, correctRounding) {
            let info = src1.info & src2.info;
            if (info === 0) {
                dst.info = 0;
                return;
            }
            let s1min = src1.min, s1max = src1.max, s2min = src2.min, s2max = src2.max;
            let p1 = s1min * s2min, p2 = s1max * s2min, p3 = s1min * s2max, p4 = s1max * s2max;
            let min = Math.min(p1, p2, p3, p4);
            let max = Math.max(p1, p2, p3, p4);
            if (correctRounding) {
                min = roundDown(min);
                max = roundUp(max);
            }
            dst.min = min;
            dst.max = max;
            dst.info = info;
        }
        /**
         * Divide two fast real intervals, sending the result to dst
         * @param src1 {FastRealInterval}
         * @param src2 {FastRealInterval}
         * @param dst {FastRealInterval}
         * @param correctRounding {boolean} Whether to use correct rounding so the result is mathematically guaranteed
         */
        static div(src1, src2, dst, correctRounding) {
            let info = src1.info & src2.info;
            if (info === 0) {
                dst.info = 0;
                return;
            }
            let s2min = src2.min, s2max = src2.max;
            if (0 < s2min || 0 > s2max) {
                // if 0 is outside the range...
                let s1min = src1.min, s1max = src1.max;
                let p1 = s1min / s2min, p2 = s1max / s2min, p3 = s1min / s2max, p4 = s1max / s2max;
                let min = Math.min(p1, p2, p3, p4);
                let max = Math.max(p1, p2, p3, p4);
                if (correctRounding) {
                    min = roundDown(min);
                    max = roundUp(max);
                }
                dst.min = min;
                dst.max = max;
                dst.info = info;
            }
            else {
                dst.min = -Infinity;
                dst.max = Infinity;
                dst.info = 1;
            }
        }
        /**
         * Take the square root of a real interval, sending the result to dst
         * @param src {FastRealInterval}
         * @param dst {FastRealInterval}
         * @param correctRounding {boolean} Whether to use correct rounding so the result is mathematically guaranteed
         */
        static sqrt(src, dst, correctRounding) {
            let info = src.info;
            if (info === 0) {
                dst.info = 0;
                return;
            }
            let min = src.min, max = src.max;
            if (max < 0) {
                dst.info = 0;
                return;
            }
            else if (max === 0) {
                dst.min = 0;
                dst.max = 0;
                dst.info = (min === 0) ? info : 1;
                return;
            }
            else {
                if (min < 0) {
                    min = 0;
                    max = Math.sqrt(max);
                    if (correctRounding)
                        max = roundUp(max);
                    info = 1;
                }
                else {
                    min = Math.sqrt(min);
                    max = Math.sqrt(max);
                    if (correctRounding) {
                        min = roundDown(min);
                        max = roundUp(max);
                    }
                }
            }
            dst.min = min;
            dst.max = max;
            dst.info = info;
        }
        /**
         * Take the cube root of a real interval, sending the result to dst
         * @param src {FastRealInterval}
         * @param dst {FastRealInterval}
         * @param correctRounding {boolean} Whether to use correct rounding so the result is mathematically guaranteed
         */
        static cbrt(src, dst, correctRounding) {
            let info = src.info;
            if (info === 0) {
                dst.info = 0;
                return;
            }
            let min = Math.cbrt(src.min);
            let max = src.min === src.max ? min : Math.cbrt(src.max);
            if (correctRounding) {
                min = roundDown(min);
                max = roundUp(max);
            }
            dst.min = min;
            dst.max = max;
            dst.info = info;
        }
        /**
         * Take the sine of a fast real interval and send the result to dst
         * @param src {FastRealInterval}
         * @param dst {FastRealInterval}
         * @param correctRounding {boolean} Whether to use correct rounding so the result is mathematically guaranteed TODO
         */
        static sin(src, dst, correctRounding) {
            const pio2 = Math.PI / 2;
            const pi3o2 = 3 * Math.PI / 2;
            const pi2 = 2 * Math.PI;
            const pi5o2 = 5 * Math.PI / 2;
            const pi7o2 = 7 * Math.PI / 2;
            let info = src.info;
            if (info === 0) {
                dst.info = 0;
                return;
            }
            let srcMin = src.min, srcMax = src.max;
            let diff = srcMax - srcMin;
            if (diff > pi2 || Number.isNaN(diff)) {
                dst.min = -1;
                dst.max = 1;
                dst.info = info;
                return;
            }
            srcMin = ((srcMin % pi2) + pi2) % pi2;
            srcMax = srcMin + diff;
            // Whether the range includes one and negative one
            let includesOne = (srcMin < pio2 && srcMax > pio2) || (srcMin < pi5o2 && srcMax > pi5o2);
            let includesNegOne = (srcMin < pi3o2 && srcMax > pi3o2) || (srcMin < pi7o2 && srcMax > pi7o2);
            if (includesOne && includesNegOne) {
                dst.min = -1;
                dst.max = 1;
                dst.info = info;
                return;
            }
            let sinSrcMin = Math.sin(srcMin);
            let sinSrcMax = Math.sin(srcMax);
            let min = includesNegOne ? -1 : Math.min(sinSrcMin, sinSrcMax);
            let max = includesOne ? 1 : Math.max(sinSrcMin, sinSrcMax);
            if (correctRounding) {
                if (min !== -1)
                    min = roundDown(min);
                if (max !== 1)
                    max = roundUp(max);
            }
            dst.min = min;
            dst.max = max;
            dst.info = info;
        }
        /**
         * Take the cosine of a fast real interval and send the result to dst
         * @param src {FastRealInterval}
         * @param dst {FastRealInterval}
         * @param correctRounding {boolean} Whether to use correct rounding so the result is mathematically guaranteed TODO
         */
        static cos(src, dst, correctRounding) {
            const pi2 = 2 * Math.PI;
            const pi3 = 3 * Math.PI;
            let info = src.info;
            if (info === 0) {
                dst.info = 0;
                return;
            }
            let srcMin = src.min, srcMax = src.max;
            let diff = srcMax - srcMin;
            if (diff > pi2 || Number.isNaN(diff)) {
                dst.min = -1;
                dst.max = 1;
                dst.info = info;
                return;
            }
            srcMin = ((srcMin % pi2) + pi2) % pi2;
            srcMax = srcMin + diff;
            // Whether the range includes one and negative one
            let includesOne = srcMin < pi2 && srcMax > pi2;
            let includesNegOne = (srcMin < Math.PI && srcMax > Math.PI) || (srcMin < pi3 && srcMax > pi3);
            if (includesOne && includesNegOne) {
                dst.min = -1;
                dst.max = 1;
                dst.info = info;
                return;
            }
            let cosSrcMin = Math.cos(srcMin);
            let cosSrcMax = Math.cos(srcMax);
            let min = includesNegOne ? -1 : Math.min(cosSrcMin, cosSrcMax);
            let max = includesOne ? 1 : Math.max(cosSrcMin, cosSrcMax);
            if (correctRounding) {
                if (min !== -1)
                    min = roundDown(min);
                if (max !== 1)
                    max = roundUp(max);
            }
            dst.min = min;
            dst.max = max;
            dst.info = info;
        }
        /**
         * Take the tangent of a fast real interval and send the result to dst
         * @param src {FastRealInterval}
         * @param dst {FastRealInterval}
         * @param correctRounding {boolean} Whether to use correct rounding so the result is mathematically guaranteed TODO
         */
        static tan(src, dst, correctRounding) {
            const pio2 = Math.PI / 2;
            let info = src.info;
            if (info === 0) {
                dst.info = 0;
                return;
            }
            let srcMin = src.min, srcMax = src.max;
            let diff = srcMax - srcMin;
            if (diff > Math.PI || Number.isNaN(diff)) {
                dst.min = -Infinity;
                dst.max = Infinity;
                dst.info = 1;
                return;
            }
            srcMin = ((srcMin % Math.PI) + Math.PI) % Math.PI;
            srcMax = srcMin + diff;
            // Whether the range includes an undef
            let includesInf = srcMin < pio2 && srcMax > pio2;
            if (includesInf) {
                dst.min = Infinity;
                dst.max = -Infinity;
                dst.info = 1;
                return;
            }
            let tanSrcMin = Math.cos(srcMin);
            let tanSrcMax = Math.cos(srcMax);
            let min = Math.min(tanSrcMin, tanSrcMax);
            let max = Math.max(tanSrcMin, tanSrcMax);
            if (correctRounding) {
                min = roundDown(min);
                max = roundUp(max);
            }
            dst.min = min;
            dst.max = max;
            dst.info = info;
        }
        /**
         * Take the arcsine of a fast real interval and send the result to dst
         * @param src {FastRealInterval}
         * @param dst {FastRealInterval}
         * @param correctRounding {boolean} Whether to use correct rounding so the result is mathematically guaranteed
         */
        static asin(src, dst, correctRounding) {
            let info = src.info;
            if (info === 0) {
                dst.info = 0;
                return;
            }
            let srcMin = src.min, srcMax = src.max;
            if (srcMax < -1 || srcMin > 1) {
                dst.info = 0;
                return;
            }
            let min, max;
            if (srcMin === srcMax) {
                min = max = Math.asin(srcMin);
            }
            else {
                if (srcMin < -1) {
                    min = -Math.PI / 2;
                    info &= 0b010;
                }
                else {
                    min = Math.asin(srcMin);
                }
                if (srcMax > 1) {
                    max = Math.PI / 2;
                    info &= 0b010;
                }
                else {
                    max = Math.asin(srcMax);
                }
            }
            if (correctRounding) {
                min = roundDown(min);
                max = roundUp(max);
            }
            dst.min = min;
            dst.max = max;
            dst.info = info;
        }
        /**
         * Take the arccosine of a fast real interval and send the result to dst
         * @param src {FastRealInterval}
         * @param dst {FastRealInterval}
         * @param correctRounding {boolean} Whether to use correct rounding so the result is mathematically guaranteed
         */
        static acos(src, dst, correctRounding) {
            let info = src.info;
            if (info === 0) {
                dst.info = 0;
                return;
            }
            let srcMin = src.min, srcMax = src.max;
            if (srcMax < -1 || srcMin > 1) {
                dst.info = 0;
                return;
            }
            let min, max;
            if (srcMin === srcMax) {
                min = max = Math.acos(srcMin);
            }
            else {
                if (srcMin < -1) {
                    max = Math.PI;
                    info &= 0b010;
                }
                else {
                    max = Math.acos(srcMin);
                }
                if (srcMax > 1) {
                    min = 0;
                    info &= 0b010;
                }
                else {
                    min = Math.acos(srcMax);
                }
            }
            if (correctRounding) {
                min = min === 0 ? min : roundDown(min);
                max = roundUp(max);
            }
            dst.min = min;
            dst.max = max;
            dst.info = info;
        }
        /**
         * Take the arctangent of a fast real interval and send the result to dst
         * @param src {FastRealInterval}
         * @param dst {FastRealInterval}
         * @param correctRounding {boolean} Whether to use correct rounding so the result is mathematically guaranteed
         */
        static atan(src, dst, correctRounding) {
            let info = src.info;
            if (info === 0) {
                dst.info = 0;
                return;
            }
            let srcMin = src.min, srcMax = src.max;
            let min = Math.atan(srcMin);
            let max = Math.atan(srcMax);
            if (correctRounding) {
                min = roundDown(min);
                max = roundUp(max);
            }
            dst.min = min;
            dst.max = max;
            dst.info = info;
        }
        /**
         * The power operation on two integers. This is a mathematical pow, rather than powSpecial where we try to guess that
         * numbers are rational. This function is a bit intricate because it needs to take care of a lot of cases. 0^0 = 0.
         * @param src1 {FastRealInterval}
         * @param src2 {FastRealInterval}
         * @param dst {FastRealInterval}
         * @param correctRounding {boolean}
         */
        static pow(src1, src2, dst, correctRounding) {
            let info = src1.info & src2.info;
            if (info === 0) {
                dst.info = 0;
                return;
            }
            let s1min = src1.min, s1max = src1.max, s2min = src2.min, s2max = src2.max;
            // First special case: exponent is a single number
            if (s2min === s2max) {
                let exp = s2min;
                let containsZero = (s1min <= 0 && s1max >= 0);
                if (exp === 0) { // exponent 0
                    dst.min = 1;
                    dst.max = 1;
                    dst.info = info;
                    return;
                }
                // There are six cases to consider: x^2-like, x^3-like, sqrt(x)-like, 1/x-like, 1/x^2-like, and 1/sqrt(x)-like.
                // Suppose those cases are enumerated 0 through 5. Then positive even integers -> 0, positive odd integers -> 1,
                // negative odd integers -> 3, negative even integers -> 4, positive numbers -> 2, negative numbers -> 5.
                if (Number.isInteger(exp)) {
                    if (containsZero) {
                        if (exp < 0) {
                            // negative even integers: minimum is the pow of the maximum
                            if (exp % 2 === 0) {
                                // 1/x^2
                                dst.min = Math.pow(Math.max(Math.abs(s1min), Math.abs(s1max)), exp);
                                dst.max = Infinity;
                                dst.info = info & 0b010;
                            }
                            else {
                                // Odd integers: if contains zero, contains an asymptote
                                // 1/x
                                dst.min = Infinity;
                                dst.max = Infinity;
                                dst.info = info & 0b010;
                            }
                            return;
                        }
                        else if (exp % 2 === 0) {
                            // x^2
                            dst.min = 0;
                            let max = Math.pow(Math.max(Math.abs(s1min), Math.abs(s1max)), exp);
                            if (correctRounding)
                                max = roundUp(max);
                            dst.max = max;
                            dst.info = info;
                            return;
                        }
                    } // containsZero
                }
                else {
                    // Exponent is not an integer
                    // Totally in the undefined region
                    let isDefined = s1max > 0;
                    if (!isDefined) {
                        dst.info = 0;
                        return;
                    }
                    if (containsZero) {
                        let min = 0, max = 0;
                        if (exp < 0) {
                            // 1/sqrt(x)
                            max = Infinity;
                            min = Math.pow(s1max, exp);
                            if (correctRounding)
                                min = roundDown(min);
                        }
                        else {
                            min = 0;
                            max = Math.pow(s1max, exp);
                            if (correctRounding)
                                max = roundUp(max);
                        }
                        dst.min = min;
                        dst.max = max;
                        dst.info = info & 0b010;
                    }
                }
                // If we've fallen through to here, pow is monotonic and defined
                let powSrcMin = Math.pow(s1min, exp), powSrcMax = Math.pow(s1max, exp);
                let min = Math.min(powSrcMin, powSrcMax);
                let max = Math.max(powSrcMin, powSrcMax);
                if (correctRounding) {
                    min = roundDown(min);
                    max = roundUp(max);
                }
                dst.min = min;
                dst.max = max;
                dst.info = info;
                return;
            } // single-valued exponent
            // Second special case: denominator is a single number
            if (s1min === s1max) {
                let base = s1min;
                if (base === 0) {
                    let containsZero = s2max >= 0 && s2min <= 0;
                    dst.min = 0;
                    dst.max = containsZero ? 1 : 0; // 0^0 = 1
                    dst.info = info & (containsZero ? 0b011 : 0b111);
                    return;
                }
                else if (base === 1) {
                    dst.min = 1;
                    dst.max = 1;
                    dst.info = info;
                    return;
                }
                else if (base === -1) {
                    dst.min = -1;
                    dst.max = 1;
                    dst.info = info & 0b010;
                    return;
                }
                // negative bases are weird. They have two branches depending on the denominator of the power they're being
                // raised to... so we deal with it :)
                let min = 0, max = 0;
                if (base < -1) {
                    // Shape: (-2)^x
                    let m = Math.pow(base, s2max);
                    min = -m;
                    max = m;
                    info &= 0b010;
                }
                else if (base < 0) {
                    // Shape: (-1/2)^x
                    let m = Math.pow(base, s2min);
                    min = -m;
                    max = m;
                    info &= 0b010;
                }
                else if (base < 1) {
                    // Monotonically decreasing
                    min = Math.pow(base, s2max);
                    max = Math.pow(base, s2min);
                }
                else {
                    // Monotonically increasing
                    min = Math.pow(base, s2min);
                    max = Math.pow(base, s2max);
                }
                if (correctRounding) {
                    min = roundDown(min);
                    max = roundUp(max);
                }
                dst.min = min;
                dst.max = max;
                dst.info = info;
                return;
            }
            // If we've gotten here, things are a bit tricky. The key is that the minimum may be -Infinity, 0, or the minimum of
            // evaluated pows, and the maximum may be 0, Infinity, or the maximum of evaluated pows. Because this is a fast
            // real interval we don't have to get deep into the weeds of which are the actual attainable intervals (though that
            // isn't *too* hard)
            let hasAsymptote = s2min <= 0 && 0 <= s2max && s1min <= 0;
            if (hasAsymptote) {
                dst.min = -Infinity;
                dst.max = Infinity;
                dst.info = info & 0b010;
                return;
            }
            // Things are potentially undefined iff the denominator has negative numbers.
            let isAllDefined = s1min < 0;
            if (isAllDefined)
                info &= 0b010;
            let minPow = Infinity, maxPow = -Infinity;
            let ps1mins2min = Math.pow(Math.abs(s1min), s2min);
            let ps1mins2max = Math.pow(Math.abs(s1min), s2max);
            let ps1maxs2min = Math.pow(Math.abs(s1max), s2min);
            let ps1maxs2max = Math.pow(Math.abs(s1max), s2max);
            minPow = Math.min(minPow, ps1mins2min);
            maxPow = Math.max(maxPow, ps1mins2min);
            minPow = Math.min(minPow, ps1mins2max);
            maxPow = Math.max(maxPow, ps1mins2max);
            if (s1min < 0) {
                minPow = Math.min(minPow, -ps1mins2min);
                maxPow = Math.max(maxPow, -ps1mins2min);
                minPow = Math.min(minPow, -ps1mins2max);
                maxPow = Math.max(maxPow, -ps1mins2max);
            }
            minPow = Math.min(minPow, ps1maxs2min);
            maxPow = Math.max(maxPow, ps1maxs2min);
            minPow = Math.min(minPow, ps1maxs2max);
            maxPow = Math.max(maxPow, ps1maxs2max);
            if (s2min < 0) {
                minPow = Math.min(minPow, -ps1maxs2min);
                maxPow = Math.max(maxPow, -ps1maxs2min);
                minPow = Math.min(minPow, -ps1maxs2max);
                maxPow = Math.max(maxPow, -ps1maxs2max);
            }
            if (correctRounding) {
                minPow = roundDown(minPow);
                maxPow = roundUp(maxPow);
            }
            dst.min = minPow;
            dst.max = maxPow;
            dst.info = info;
        }
    }

    // lookup[i] = gamma(i)
    const gammaLookup = Object.freeze([
        NaN, 1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800, 39916800, 479001600, 6227020800, 87178291200, 1307674368000,
        20922789888000, 355687428096000, 6402373705728000,
        // TODO ensure accuracy to 1 ulp
        121645100408832000, 2432902008176640000, 51090942171709440000,
        1.1240007277776077e+21, 2.585201673888498e+22, 6.204484017332394e+23, 1.5511210043330986e+25, 4.0329146112660565e+26,
        1.0888869450418352e+28, 3.0488834461171384e+29, 8.841761993739701e+30, 2.6525285981219103e+32, 8.222838654177922e+33,
        2.631308369336935e+35, 8.683317618811886e+36, 2.9523279903960412e+38, 1.0333147966386144e+40, 3.719933267899012e+41,
        1.3763753091226343e+43, 5.23022617466601e+44, 2.0397882081197442e+46, 8.159152832478977e+47, 3.3452526613163803e+49,
        1.4050061177528798e+51, 6.041526306337383e+52, 2.6582715747884485e+54, 1.1962222086548019e+56, 5.5026221598120885e+57,
        2.5862324151116818e+59, 1.2413915592536073e+61, 6.082818640342675e+62, 3.0414093201713376e+64, 1.5511187532873822e+66,
        8.065817517094388e+67, 4.2748832840600255e+69, 2.308436973392414e+71, 1.2696403353658276e+73, 7.109985878048635e+74,
        4.052691950487722e+76, 2.350561331282879e+78, 1.3868311854568986e+80, 8.320987112741392e+81, 5.075802138772248e+83,
        3.146997326038794e+85, 1.98260831540444e+87, 1.2688693218588417e+89, 8.247650592082472e+90, 5.443449390774431e+92,
        3.647111091818868e+94, 2.4800355424368305e+96, 1.711224524281413e+98, 1.197857166996989e+100, 8.504785885678622e+101,
        6.123445837688608e+103, 4.4701154615126834e+105, 3.3078854415193856e+107, 2.480914081139539e+109, 1.8854947016660498e+111,
        1.4518309202828584e+113, 1.1324281178206295e+115, 8.946182130782973e+116, 7.156945704626378e+118, 5.797126020747366e+120,
        4.75364333701284e+122, 3.945523969720657e+124, 3.314240134565352e+126, 2.8171041143805494e+128, 2.4227095383672724e+130,
        2.107757298379527e+132, 1.8548264225739836e+134, 1.6507955160908452e+136, 1.4857159644817607e+138, 1.3520015276784023e+140,
        1.24384140546413e+142, 1.1567725070816409e+144, 1.0873661566567424e+146, 1.0329978488239052e+148, 9.916779348709491e+149,
        9.619275968248206e+151, 9.426890448883242e+153, 9.33262154439441e+155, 9.33262154439441e+157, 9.425947759838354e+159,
        9.614466715035121e+161, 9.902900716486175e+163, 1.0299016745145622e+166, 1.0813967582402903e+168, 1.1462805637347078e+170,
        1.2265202031961373e+172, 1.3246418194518284e+174, 1.4438595832024928e+176, 1.5882455415227421e+178, 1.7629525510902437e+180,
        1.9745068572210728e+182, 2.2311927486598123e+184, 2.543559733472186e+186, 2.925093693493014e+188, 3.3931086844518965e+190,
        3.969937160808719e+192, 4.6845258497542883e+194, 5.574585761207603e+196, 6.689502913449124e+198, 8.09429852527344e+200,
        9.875044200833598e+202, 1.2146304367025325e+205, 1.5061417415111404e+207, 1.8826771768889254e+209, 2.372173242880046e+211,
        3.012660018457658e+213, 3.8562048236258025e+215, 4.9745042224772855e+217, 6.466855489220472e+219, 8.471580690878817e+221,
        1.118248651196004e+224, 1.4872707060906852e+226, 1.992942746161518e+228, 2.6904727073180495e+230, 3.659042881952547e+232,
        5.01288874827499e+234, 6.917786472619486e+236, 9.615723196941086e+238, 1.346201247571752e+241, 1.89814375907617e+243,
        2.6953641378881614e+245, 3.8543707171800706e+247, 5.550293832739301e+249, 8.047926057471987e+251, 1.17499720439091e+254,
        1.7272458904546376e+256, 2.5563239178728637e+258, 3.808922637630567e+260, 5.7133839564458505e+262, 8.627209774233235e+264,
        1.3113358856834518e+267, 2.006343905095681e+269, 3.089769613847349e+271, 4.789142901463391e+273, 7.47106292628289e+275,
        1.1729568794264138e+278, 1.8532718694937338e+280, 2.946702272495037e+282, 4.714723635992059e+284, 7.590705053947215e+286,
        1.2296942187394488e+289, 2.0044015765453015e+291, 3.2872185855342945e+293, 5.423910666131586e+295, 9.003691705778433e+297,
        1.5036165148649983e+300, 2.526075744973197e+302, 4.2690680090047027e+304, 7.257415615307994e+306
    ]);
    const lookupLen = 172;
    // g = 7
    const lanczosCoefficients = Object.freeze([
        676.5203681218851, -1259.1392167224028, 771.32342877765313, -176.61502916214059, 12.507343278686905, -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7
    ]);
    /**
     * Gamma function, extension of the factorial function. Credit to Frederick Johannson; this uses the Lanczos
     * approximation (see https://en.wikipedia.org/wiki/Lanczos_approximation).
     *
     * NaN -> NaN, Infinity -> Infinity, -Infinity -> NaN
     *
     * Geometric mean of error: 11 ulp (particularly bad near poles, as one would expect)
     * @param x Any floating-point number
     * @returns Approximation to gamma(x)
     */
    function gammaReal(x) {
        x = +x;
        if (!Number.isFinite(x))
            return Infinity + x;
        if (Number.isInteger(x)) {
            // Use lookup table for extra precision
            if (x <= 0)
                return NaN;
            if (x >= lookupLen)
                return Infinity;
            return gammaLookup[x];
        }
        if (x < 0.5) {
            // Reflection formula
            return Math.PI / (Math.sin(Math.PI * x) * gammaReal(1 - x));
        }
        x -= 1;
        let s = 0.99999999999980993;
        for (let i = 0; i < 8; ++i) {
            s += lanczosCoefficients[i] / (x + i + 1);
        }
        let t = x + 7.5; // 8 - 0.5
        // yeah idk
        return 2.5066282746310007 * Math.pow(t, x + 0.5) * Math.exp(-t) * s;
    }
    function lnGammaReal(x) {
        x = +x;
        if (!Number.isFinite(x))
            return Infinity + x;
        if (Number.isInteger(x)) {
            if (x <= 0)
                return NaN;
        }
        if (x < 0.5) {
            // Reflection formula
            return 1.1447298858494002 - (Math.log(Math.sin(Math.PI * x))) - lnGammaReal(1 - x);
        }
        x -= 1;
        let s = 0.99999999999980993;
        for (let i = 0; i < 8; ++i) {
            s += lanczosCoefficients[i] / (x + i + 1);
        }
        let t = x + 7.5; // 8 - 0.5
        return 0.9189385332046728 + Math.log(t) * (x + 0.5) - t + Math.log(s);
    }

    /**
     * Normal-precision complex number.
     */
    class Complex {
        constructor(re = 0, im = 0) {
            this.re = +re;
            this.im = +im;
        }
        /**
         * Attempt to parse an object. Returns a Complex with NaN parts if couldn't sensibly convert.
         * @param o {any}
         * @return {Complex}
         */
        static fromObj(o) {
            let re = NaN, im = NaN;
            if (typeof o === "number") {
                re = o;
                im = 0;
            }
            else if (typeof o === "object") {
                if (Array.isArray(o)) {
                    re = +o[0];
                    im = +o[1];
                }
                else if (o.x !== undefined && o.y !== undefined) {
                    re = +o.x;
                    im = +o.y;
                }
                else if (o.re !== undefined && o.im !== undefined) {
                    re = +o.re;
                    im = +o.im;
                }
            }
            return new Complex(re, im);
        }
        /**
         * Add two complex numbers and write the result to this complex number.
         * @param c1
         * @param c2
         */
        add(c1, c2) {
            let c1r = c1.re, c2r = c2.re, c1i = c1.im, c2i = c2.im;
            this.re = c1r + c2r;
            this.im = c1i + c2i;
        }
        multiply(c1, c2) {
            let c1r = c1.re, c2r = c2.re, c1i = c1.im, c2i = c2.im;
            this.re = c1r * c2r - c1i * c2i;
            this.im = c1r * c2i + c1i * c2r;
        }
        multiplyReal(c, r) {
            this.re = c.re * r;
            this.im = c.im * r;
        }
        divide(c1, c2) {
            let c1r = c1.re, c2r = c2.re, c1i = c1.im, c2i = c2.im;
            let d = 1 / (c2r * c2r + c2i * c2i);
            this.re = (c1r * c2r + c1i * c2i) * d;
            this.im = (c1i * c2r - c1r * c2i) * d;
        }
        reciprocal(c) {
            let cr = c.re, ci = c.im;
            let denom = 1 / (cr * cr + ci * ci);
            this.re = cr * denom;
            this.im = -ci * denom;
        }
        subtract(c1, c2) {
            let c1r = c1.re, c2r = c2.re, c1i = c1.im, c2i = c2.im;
            this.re = c1r - c2r;
            this.im = c1i - c2i;
        }
        sin(c) {
            let cr = c.re, ci = c.im;
            this.re = Math.sin(cr) * Math.cosh(ci);
            this.im = Math.cos(cr) * Math.sinh(ci);
        }
        cos(c) {
            let cr = c.re, ci = c.im;
            this.re = Math.cos(cr) * Math.cosh(ci);
            this.im = -Math.sin(cr) * Math.sinh(ci);
        }
        tan(c) {
            let cr = c.re, ci = c.im;
            let denom = 1 / (Math.cos(2 * cr) + Math.cosh(2 * ci));
            this.re = Math.sin(2 * cr) * denom;
            this.im = Math.sinh(2 * ci) * denom;
        }
        exp(c) {
            let cr = c.re, ci = c.im;
            let exp = Math.exp(cr);
            this.re = exp * Math.cos(ci);
            this.im = exp * Math.sin(ci);
        }
        log(c) {
            let cr = c.re, ci = c.im;
            this.re = Math.log(Math.hypot(c.re, ci));
            this.im = Math.atan2(c.im, cr);
        }
        pow(c1, c2) {
            let c2i = c2.im;
            if (c2i === 0) {
                this.powReal(c1, c2.re);
                return;
            }
            let c = new Complex();
            c.log(c1);
            c.multiply(c2, c);
            this.exp(c);
        }
        /**
         * Set this complex number to the result of b^r
         * @param b {Complex}
         * @param r {number}
         */
        powReal(b, r) {
            if (Number.isInteger(r)) {
                if (r === 0) { // TODO add more integer handling
                    this.re = 1;
                    this.im = 0;
                    return;
                }
            }
            let c = new Complex();
            c.log(b);
            c.multiplyReal(c, r);
            this.exp(c);
        }
        static abs(c) {
            return Math.hypot(c.re, c.im); // kinda slow, but whatevs
        }
        static absSquared(c) {
            return c.re * c.re + c.im * c.im;
        }
        static arg(c) {
            return Math.atan2(c.im, c.re);
        }
        /**
         * Get the approximate argument of a complex number, quickly (for applications like domain coloring, where accuracy
         * is not essential)
         * @param c {Complex}
         * @returns {number}
         */
        static approxArg(c) {
            // Credit to https://math.stackexchange.com/a/1105038/677124
            let x = c.im, y = c.re;
            if (!Number.isFinite(x) || !Number.isFinite(y)) {
                return Math.atan2(y, x); // will probably be fast bc of special handlers within atan2
            }
            let absX = Math.abs(x), absY = Math.abs(y);
            let a = (absX > absY) ? (absY / absX) : (absX / absY);
            let s = a * a;
            let r = ((-0.0464964749 * s + 0.15931422) * s - 0.327622764) * s * a + a;
            if (absY > absX)
                r = Math.PI / 2 - r;
            if (x < 0)
                r = Math.PI - r;
            if (y < 0)
                r = -r;
            return r;
        }
        gamma(z) {
            let zi = z.im, zr = z.re;
            if (Math.abs(zi) < 1e-15) {
                this.re = gammaReal(zr);
                this.im = 0;
                return;
            }
            if (!Number.isFinite(zi) || !Number.isFinite(zr)) {
                if (zi !== zi || zr !== zr) {
                    this.re = this.im = NaN;
                    return;
                }
                // TODO handle cases
                this.re = this.im = NaN;
                return;
            }
            if (zr < 0.5) {
                // Reflection formula
                let c = new Complex(1 - zr, -zi);
                let sz = new Complex();
                sz.multiplyReal(z, Math.PI);
                sz.sin(sz);
                c.gamma(c);
                c.multiply(c, sz);
                this.divide(new Complex(Math.PI, 0), c);
                return;
            }
            zr -= 1;
            let sr = 0.99999999999980993, si = 0;
            let dRe = 0, dIm = zi;
            for (let i = 0; i < 8; ++i) {
                dRe = zr + i + 1;
                // Compute coeff[i] / (dRe + i * dIm)
                let denom = lanczosCoefficients[i] / (dRe * dRe + dIm * dIm);
                sr += dRe * denom;
                si += -dIm * denom;
            }
            let t = new Complex(zr + 7.5, zi);
            let sqrt2Pi = new Complex(2.5066282746310007, 0);
            let tPow = new Complex();
            tPow.pow(t, new Complex(zr + 0.5, zi));
            let tExp = new Complex();
            tExp.exp(new Complex(-t.re, -t.im));
            t.multiply(sqrt2Pi, tPow);
            t.multiply(new Complex(sr, si), t);
            this.multiply(tExp, t);
        }
    }

    // The actual types and typecasts used by Grapheme.
    // The boolean type is nullable. meaning it takes on a value of 0, 1, or NaN. -0, false, and true are also ACCEPTED as
    // values, but aren't used that way internally, since each is implicitly casted to the correct value in all numeric
    // calculations.
    let concreteBoolean = new ConcreteType({
        name: "bool",
        isPrimitive: true,
        init: () => 0,
        // The typechecks are for usability, not strictness
        typecheck: NullableBoolean.isUsableNullableBoolean,
        typecheckVerbose: NullableBoolean.typecheckUsableNullableBoolean,
        castPermissive: x => !!x
    });
    // Integers can be any number that's not a non-integral finite number (so they can be ±Infinity, NaN) but they can
    // overflow--meaning any operation that takes them out of the [-2^53 - 1, 2^53 - 1] safe range
    let concreteInt = new ConcreteType({
        name: "int",
        isPrimitive: true,
        init: () => 0,
        typecheck: NullableInteger.isNullableInteger,
        typecheckVerbose: NullableInteger.typecheckNullableInteger,
        castPermissive: x => Math.round(x)
    });
    // Real can be ANY floating-point number
    let concreteReal = new ConcreteType({
        name: "real",
        isPrimitive: true,
        init: () => 0,
        typecheck: b => typeof b === "number",
        typecheckVerbose: b => (typeof b !== "number") ? ("Expected JS number, found type " + (typeof b)) : "",
        castPermissive: x => +x
    });
    let concreteComplex = new ConcreteType({
        name: "complex",
        isPrimitive: false,
        init: () => new Complex(0, 0),
        typecheck: b => b instanceof Complex,
        typecheckVerbose: b => (b instanceof Complex) ? "Expected complex number" : "",
        clone: c => new Complex(c.re, c.im),
        copyTo: (src, dst) => { dst.re = src.re; dst.im = src.im; },
        castPermissive: Complex.fromObj
    });
    let concreteIntervalBoolean = new ConcreteType({
        name: "interval_bool",
        isPrimitive: false,
        init: () => new FastBooleanInterval(false, false, 0b111),
        typecheck: b => b instanceof FastBooleanInterval,
        clone: b => new FastBooleanInterval(b.min, b.max, b.info),
        copyTo: (src, dst) => { dst.min = src.min; dst.max = src.max; dst.info = src.info; },
        castPermissive: () => { throw 1; }
    });
    let concreteIntervalReal = new ConcreteType({
        name: "interval_real",
        isPrimitive: false,
        init: () => new FastRealInterval(0, 0, 0b1111),
        typecheck: b => b instanceof FastRealInterval,
        clone: b => new FastRealInterval(b.min, b.max, b.info),
        copyTo: (src, dst) => { dst.min = src.min; dst.max = src.max; dst.info = src.info; },
        castPermissive: FastRealInterval.fromObj
    });
    let concreteIntervalInt = new ConcreteType(Object.assign(Object.assign({}, concreteIntervalReal), { name: "interval_int" }));
    /**
     * MATHEMATICAL TYPES
     */
    let mathematicalReal = new MathematicalType({
        name: "real"
    });
    let mathematicalInt = new MathematicalType({
        name: "int"
    });
    let mathematicalComplex = new MathematicalType({
        name: "complex"
    });
    function defineConcreteType(concreteType) {
        let { name } = concreteType;
        concreteTypes.set(name, concreteType);
    }
    function defineMathematicalType(type) {
        let { name } = type;
        mathematicalTypes.set(name, type);
    }
    /**
     * "Intelligently" convert an object to the corresponding concrete type object. Returns null if no such type is found
     * @param o {any}
     * @param throwOnError
     * @returns {ConcreteType|null}
     */
    function toConcreteType(o, throwOnError = false) {
        var _a;
        let r = null;
        if (typeof o === "string")
            r = (_a = concreteTypes.get(o)) !== null && _a !== void 0 ? _a : null;
        else
            r = (o instanceof ConcreteType) ? o : null;
        if (!r && throwOnError) {
            throw new Error("No concrete type found for " + o);
        }
        return r;
    }
    /**
     * "Intelligently" convert an object to the corresponding mathematical type object
     * @param o {*}
     * @param throwOnError
     * @returns {MathematicalType|null}
     */
    function toMathematicalType(o, throwOnError = false) {
        var _a;
        let r = null;
        if (typeof o === "string")
            r = (_a = mathematicalTypes.get(o)) !== null && _a !== void 0 ? _a : null;
        else
            r = (o instanceof MathematicalType) ? o : null;
        if (!r && throwOnError) {
            throw new Error("No mathematical type found for " + o);
        }
        return r;
    }
    let concreteTypes = new Map();
    let mathematicalTypes = new Map();
    [concreteBoolean, concreteInt, concreteReal, concreteIntervalBoolean, concreteIntervalInt, concreteIntervalReal, concreteComplex].forEach(defineConcreteType);
    [mathematicalReal, mathematicalInt, mathematicalComplex].forEach(defineMathematicalType);

    // Evaluators are functions of a specific signature that compute some operator. They are less abstract than the
    const jsPrimitives = Object.freeze(['+', '-', '/', '*', '&&', '||', '==', '!=', '<=', '>=', '<', '>']);
    const unaryPrimitives = {
        '-': x => -x
    };
    // @ts-ignore
    const binaryPrimitives = {};
    jsPrimitives.forEach(op => {
        binaryPrimitives[op] = (new Function('x', 'y', `return x ${op} y`));
    });
    class ConcreteEvaluator {
        constructor(params) {
            var _a, _b, _c, _d, _e;
            // @ts-ignore (check occurs immediately)
            this.args = ((_a = params.args) !== null && _a !== void 0 ? _a : []).map(toConcreteType);
            if (!this.args.every(arg => !!arg))
                throw new Error("Unknown argument type");
            let returns = toConcreteType((_b = params.returns) !== null && _b !== void 0 ? _b : "void");
            if (!returns)
                throw new Error("Unknown return type");
            this.returns = returns;
            this.argCount = this.args.length;
            this.identity = !!params.identity;
            this.evalType = (_c = params.evalType) !== null && _c !== void 0 ? _c : "new";
            if (this.evalType !== "new" && this.evalType !== "write") {
                throw new Error("Evaluator type must be either new or write, not " + this.evalType);
            }
            // Primitive evaluator symbol (that can basically be evaled, for example "+" in +(real, real) -> real)
            this.primitive = (_d = params.primitive) !== null && _d !== void 0 ? _d : "";
            this.func = (_e = params.func) !== null && _e !== void 0 ? _e : this.getDefaultFunc();
            this.isConstant = !!params.isConstant;
            this.applyTag();
        }
        getDefaultFunc() {
            if (this.returns.isPrimitive && this.evalType === "write")
                throw new Error("Cannot write to a primitive");
            let func = null;
            if (this.identity) {
                if (this.evalType === "new") {
                    func = this.returns.clone;
                }
                else if (this.evalType === "write") {
                    func = this.returns.copyTo;
                }
            }
            else if (this.primitive) {
                if (this.argCount === 0) {
                    func = new Function("return " + this.primitive);
                }
                else if (this.argCount === 1) {
                    func = unaryPrimitives[this.primitive];
                }
                else if (this.argCount === 2) {
                    func = binaryPrimitives[this.primitive];
                }
                this.evalType = "new";
            }
            if (!func)
                throw new Error("Unable to generate evaluation function");
            return func;
        }
        applyTag() {
        }
        /**
         * Given a list of concrete types, whether the evaluator can be called with those types. -1 if not, 0 if no casts are
         * needed, >0 for the number of needed casts
         * @param args
         */
        castDistance(args) {
            let casts = this.getCasts(args);
            if (!casts)
                return -1;
            return castDistance(casts);
        }
        getCasts(args) {
            if (this.args.length !== args.length)
                return null;
            let casts = [];
            for (let i = 0; i < args.length; ++i) {
                let cast = getConcreteCast(args[i] /* src */, this.args[i]);
                if (!cast)
                    return null;
                casts.push(cast);
            }
            return casts;
        }
        /**
         * Call an evaluator as if it were new (creating a new value if it's a writes evaluator)
         * @param args
         */
        callNew(args) {
            if (this.evalType === "new") {
                return this.func(...args);
            }
            let writeTo = this.returns.init();
            this.func(...args, writeTo);
            return writeTo;
        }
    }
    // A concrete cast is a special evaluator taking in one concrete type and outputting another. Note the concrete types
    // might be of the same mathematical type! For example, the concrete cast real -> real_interval makes quite a bit of
    // sense; 3.2 -> [3.2, 3.2].
    class ConcreteCast extends ConcreteEvaluator {
        constructor(params) {
            if (!params.src || !params.dst)
                throw new Error("No source or destination types provided");
            params.args = [params.src];
            params.returns = params.dst;
            super(params);
        }
        srcType() {
            return this.args[0];
        }
        dstType() {
            return this.returns;
        }
        /**
         * Whether this cast is a logical identity cast (still differentiating between types that have the same
         * underlying type, like real and int)
         * @returns {boolean}
         */
        isIdentity() {
            return false;
        }
    }
    // TODO logical concrete casts for each concrete type
    class IdentityConcreteCast extends ConcreteCast {
        isIdentity() {
            return true;
        }
        callNew(args) {
            return args[0];
        }
    }
    let I = new IdentityConcreteCast({
        // TODO
        src: "int",
        dst: "int",
        func: x => x
    });
    // src? -> dst? -> cast?
    const BUILTIN_CONCRETE_CASTS = new Map();
    /**
     * Register a concrete cast from src to dst
     * @param cast
     */
    function registerConcreteCast(cast) {
        const CASTS = BUILTIN_CONCRETE_CASTS;
        let srcType = cast.srcType().toHashStr();
        let dstType = cast.dstType().toHashStr();
        if (!CASTS.has(srcType))
            CASTS.set(srcType, new Map());
        let srcCasts = CASTS.get(srcType); // set above
        srcCasts.set(dstType, cast);
        return cast;
    }
    /**
     * Get cast from src to dst. Returns "null" if the cast doesn't exist, "identity" if the types are the same, and a
     * corresponding ConcreteCast if there is a match
     * @param srcType
     * @param dstType
     */
    function getConcreteCast(srcType, dstType) {
        var _a;
        if (srcType.isSameConcreteType(dstType))
            return I;
        let srcCasts = BUILTIN_CONCRETE_CASTS.get(srcType.toHashStr());
        if (!srcCasts)
            return null;
        return (_a = srcCasts.get(dstType.toHashStr())) !== null && _a !== void 0 ? _a : null;
    }
    /**
     * Determine the number of non-identity casts in a list of casts, and -1 if there is an empty cast somewhere
     * @param casts
     * @returns {number}
     */
    function castDistance(casts) {
        let count = 0;
        for (let cast of casts) {
            if (!cast.isIdentity())
                count++;
            else if (!cast) {
                count = -1;
                break;
            }
        }
        return count;
    }

    class EvaluationMode {
        constructor(name, params) {
            var _a;
            this.name = name;
            this.args = (_a = params.args) !== null && _a !== void 0 ? _a : [];
            this.argCount = this.args.length;
            /**
             * Mapping between mathematical type hash strings and their concrete types
             * @type {Map<string, ConcreteType>}
             */
            this.typeMap = new Map();
            this.fillTypeMap(params.typeMap);
        }
        fillTypeMap(m) {
            for (let [mathematical, concrete] of Object.entries(m)) {
                this.typeMap.set(mathematical, toConcreteType(concrete, true));
            }
        }
        /**
         * Get the concrete type associated with the mathematical type. Returns null if no type was found.
         */
        getConcreteType(mType) {
            var _a;
            return (_a = this.typeMap.get(mType.name)) !== null && _a !== void 0 ? _a : null;
        }
        toString() {
            return this.name;
        }
    }
    const normal = new EvaluationMode("normal", {
        typeMap: {
            "int": "int",
            "real": "real",
            "bool": "bool",
            "complex": "complex"
        }
    });
    const fastInterval = new EvaluationMode("fast_interval", {
        typeMap: {
        //"int": "fast_int_interval",
        //"real": "fast_real_interval",
        //"bool": "fast_bool_interval"
        }
    });
    const EvaluationModes = new Map();
    EvaluationModes.set("normal", normal);
    EvaluationModes.set("fast_interval", fastInterval);
    /**
     * Convert the argument, either a string or an evaluation mode, to the corresponding evaluation mode
     * @param o Any object
     * @param throwOnError Whether to throw a descriptive error
     */
    function toEvaluationMode(o, throwOnError = true) {
        var _a;
        if (o instanceof EvaluationMode)
            return o;
        let mode = (_a = EvaluationModes.get(o)) !== null && _a !== void 0 ? _a : null;
        if (!mode && throwOnError) {
            if (typeof o === "string") {
                throw new Error("Unrecognized evaluation mode " + o);
            }
            else {
                throw new Error("Evaluation mode must be a string ('normal') or EvaluationMode object");
            }
        }
        return mode;
    }

    /**
     * Attempt conversion from array of types to corresponding mathematical types
     * @param args {any}
     * @returns MathematicalType[]
     */
    function convertArgumentTypes(args) {
        if (args == null)
            return [];
        if (!Array.isArray(args))
            throw new TypeError("Expected argument type list to be an array");
        let converted = args.map(a => toMathematicalType(a, true));
        return converted; // null checks done
    }
    class OperatorDefinition {
        constructor(params) {
            var _a, _b;
            /**
             * Readable name of the operator that identifies it: e.g., "^", "/", "gamma"
             * @type {string}
             */
            this.name = params.name;
            /**
             * Arguments
             * @type {MathematicalType[]}
             */
            this.args = convertArgumentTypes(params.args);
            /**
             * Return type (void type if nothing)
             * @type {MathematicalType}
             */
            this.returns = toMathematicalType((_a = params.returns) !== null && _a !== void 0 ? _a : "void", true);
            if (!this.returns) {
                throw new Error(`Unknown return type (attempted conversion from ${params.returns})`);
            }
            /**
             * List of concrete evaluators that may be searched through
             */
            this.evaluators = (_b = params.evaluators) !== null && _b !== void 0 ? _b : [];
            this.tags = {
                builtin: !!params.builtin,
                constant: !!params.constant // whether function is totally constant (e.g., x => 1)
            };
            if (params.tags)
                Object.assign(this.tags, params.tags);
            this.defaultEvaluators = new Map();
            this.fillDefaultEvaluators();
        }
        /**
         * Compute evaluators for each mode, a (not necessarily strict) subset of all available evaluators. These define the
         * "most canonical" evaluation of the expression; the default evaluator of a given mode is the one that should be
         * called in an evaluate() invocation, for example.
         */
        fillDefaultEvaluators() {
            let evaluators = this.evaluators, args = this.args;
            let possibleSignatures = Array.from(EvaluationModes.values()).map(mode => ({
                mode,
                returns: mode.getConcreteType(this.returns),
                args: args.map(mt => mode.getConcreteType(mt))
            })).filter(({ returns, args }) => returns !== null && args.every(a => a !== null)); // eliminate signatures w/ missing types
            for (let p of possibleSignatures) {
                let { mode, returns, args } = p;
                let foundEvaluator;
                for (let e of evaluators) {
                    // See if the evaluator matches the signature. null returns and args are filtered out above
                    if (e.returns.isSameConcreteType(returns) &&
                        args.every((arg, i) => arg.isSameConcreteType(e.args[i]))) {
                        foundEvaluator = e;
                        if (e.evalType === "new") { // prefer "new" evaluators
                            break;
                        }
                    }
                }
                if (foundEvaluator) {
                    this.defaultEvaluators.set(mode.name, foundEvaluator);
                }
            }
        }
        /**
         * Get the default evaluator for a given mode (null if doesn't exist)
         * @param mode {EvaluationMode}
         * @returns {ConcreteEvaluator|null}
         */
        getDefaultEvaluator(mode) {
            var _a;
            return (_a = this.defaultEvaluators.get(mode.name)) !== null && _a !== void 0 ? _a : null;
        }
        findEvaluator(args, preferences) {
            let evaluators = this.evaluators;
            let { evalType } = preferences;
            let best = null;
            for (let i = 0; i < evaluators.length; ++i) {
                let e = evaluators[i];
                let dist = e.castDistance(args);
                if (dist === 0) {
                    best = e;
                    if (e.evalType === evalType) {
                        break;
                    }
                }
            }
            return best;
        }
        /**
         * Check whether this operator can be called with the given mathematical types.
         * @param args
         * @returns -1 if it cannot be called, a nonnegative integer giving the number of necessary implicit casts to call it
         */
        canCallWith(args) {
            let casts = this.getCasts(args);
            if (!casts)
                return -1;
            return castDistance(casts);
        }
        /**
         * Get a list of mathematical casts from source types to the required types for this operator.
         * @param args
         */
        getCasts(args) {
            if (this.args.length !== args.length)
                return null;
            let casts = [];
            for (let i = 0; i < args.length; ++i) {
                let cast = getMathematicalCast(args[i] /* src */, this.args[i]);
                if (!cast)
                    return null;
                casts.push(cast);
            }
            return casts;
        }
        prettyPrint() {
            // ^(int, int) -> int
            return `${this.name}(${this.args.map(arg => arg.prettyPrint()).join(', ')}) -> ${this.returns.prettyPrint()}`;
        }
    }
    /**
     * A mathematical cast is just a special operator that converts one type to another, accepting a single argument and
     * returning the destination type
     */
    class MathematicalCast extends OperatorDefinition {
        constructor(params) {
            var _a;
            if (!params.src || !params.dst)
                throw new Error("No source or destination types provided");
            let nParams = Object.assign(Object.assign({}, params), { name: params.src.toString(), args: [params.src], returns: params.dst });
            super(nParams);
            this.name = (_a = this.name) !== null && _a !== void 0 ? _a : this.returns.toHashStr();
        }
        /**
         * Source type
         */
        srcType() {
            return this.args[0];
        }
        /**
         * Destination type
         * @returns {MathematicalType}
         */
        dstType() {
            return this.returns;
        }
        /**
         * Whether this cast IS the identity cast. This function distinguishes the single, canonical identity cast
         * (which should only be used between objects of identical type) and everything else, including "mathematically
         * identical" casts like int -> real.
         * @returns {boolean}
         */
        isIdentity() {
            return false;
        }
    }
    class IdentityMathematicalCast extends MathematicalCast {
        isIdentity() {
            return true;
        }
    }
    /**
     * Identity casts generated on a per-type basis (somewhat of a formalism; in a compiled setting these will all be elided)
     * @type {Map<string, IdentityMathematicalCast>}
     */
    const CachedIdentityCasts = new Map();
    /**
     * Generate formal identity evaluators for a given mathematical cast
     * @param srcType
     * @returns
     */
    function generateIdentityEvaluators(srcType) {
        let evaluators = [];
        for (let mode of EvaluationModes.values()) {
            let concreteType = mode.getConcreteType(srcType);
            if (concreteType != null) // only get casts for which there are corresponding concrete types
                evaluators.push(new ConcreteCast({
                    identity: true,
                    src: concreteType,
                    dst: concreteType,
                    func: x => x /* TODO: examine logical consistency here */
                }));
        }
        return evaluators;
    }
    /**
     * Generate a cached IdentityCast object for the given source type
     * @param srcType
     */
    function generateIdentityCast(srcType) {
        let s = srcType.toHashStr();
        let c = CachedIdentityCasts.get(s);
        if (!c) {
            CachedIdentityCasts.set(s, c = new IdentityMathematicalCast({
                src: srcType,
                dst: srcType,
                evaluators: generateIdentityEvaluators(srcType)
            }));
        }
        return c;
    }
    /**
     * First map key is source type; second map key is destination type
     */
    const BuiltinMathematicalCasts = new Map();
    /**
     * Register a mathematical cast from src to dst
     * @param cast
     */
    function registerMathematicalCast(cast) {
        const CASTS = BuiltinMathematicalCasts;
        let srcType = cast.srcType().toHashStr();
        let dstType = cast.dstType().toHashStr();
        if (!CASTS.has(srcType))
            CASTS.set(srcType, new Map());
        let srcCasts = CASTS.get(srcType);
        srcCasts.set(dstType, cast);
        return cast;
    }
    /**
     * Get cast from src to dst. Returns "null" if the cast doesn't exist, "identity" if the types are the same, and a
     * corresponding MathematicalCast if there is a match
     * @param srcType
     * @param dstType
     */
    function getMathematicalCast(srcType, dstType) {
        var _a;
        if (!(srcType instanceof MathematicalType) || !(dstType instanceof MathematicalType))
            throw new Error("Invalid source or destination type");
        if (srcType.isSameType(dstType))
            return generateIdentityCast(srcType);
        let srcCasts = BuiltinMathematicalCasts.get(srcType.toHashStr());
        if (!srcCasts)
            return null;
        return (_a = srcCasts.get(dstType.toHashStr())) !== null && _a !== void 0 ? _a : null;
    }

    // For now we'll just have a mapping  name -> Array of possibilities
    const KNOWN_OPERATORS = new Map();
    // Put an operator in the global list
    function registerOperator(definition) {
        let name = definition.name;
        if (!KNOWN_OPERATORS.has(name))
            KNOWN_OPERATORS.set(name, []);
        KNOWN_OPERATORS.get(name).push(definition);
    }
    /**
     * Get the corresponding OperatorDefinition of a given name and arg types, returning [ definition, casts ] on success,
     * where casts is an array of MathematicalCasts, and [ null, null ] on failure
     * @param name
     * @param argTypes
     */
    function resolveOperatorDefinition(name, argTypes) {
        let defs = KNOWN_OPERATORS.get(name);
        if (!defs)
            return [null, null];
        // Choose first definition with the least cast distance (may change later)
        let bestDef = null;
        let bestCasts = null;
        let bestDist = Infinity;
        for (let def of defs) {
            let casts = def.getCasts(argTypes);
            if (casts) {
                let dist = castDistance(casts);
                if (dist < bestDist) {
                    bestDef = def;
                    bestCasts = casts;
                    bestDist = dist;
                }
                if (dist === 0) // if no casting is necessary, that's clearly the best
                    break;
            }
        }
        return bestDef ? [bestDef, bestCasts] : [null, null];
    }
    /**
     * Basic arithmetic operations TODO: extended names, etc.
     */
    /**
     * Integer operations
     */
    registerOperator(new OperatorDefinition({
        name: '+',
        args: ["int", "int"],
        returns: "int",
        evaluators: [
            new ConcreteEvaluator({
                args: ["int", "int"],
                returns: "int",
                primitive: "+"
            })
        ]
    }));
    registerOperator(new OperatorDefinition({
        name: '*',
        args: ["int", "int"],
        returns: "int",
        evaluators: [
            new ConcreteEvaluator({
                args: ["int", "int"],
                returns: "int",
                primitive: "*"
            })
        ]
    }));
    registerOperator(new OperatorDefinition({
        name: '-',
        args: ["int", "int"],
        returns: "int",
        evaluators: [
            new ConcreteEvaluator({
                args: ["int", "int"],
                returns: "int",
                primitive: "-"
            })
        ]
    }));
    // Unary minus
    registerOperator(new OperatorDefinition({
        name: '-',
        args: ["int"],
        returns: "int",
        evaluators: [
            new ConcreteEvaluator({
                args: ["int"],
                returns: "int",
                primitive: "-"
            })
        ]
    }));
    // int(int)
    registerOperator(new OperatorDefinition({
        name: 'int',
        args: ["int"],
        returns: "int",
        evaluators: [
            new ConcreteEvaluator({
                args: ["int"],
                returns: "int",
                identity: true
            })
        ]
    }));
    /**
     * Real operators
     */
    registerOperator(new OperatorDefinition({
        name: '+',
        args: ["real", "real"],
        returns: "real",
        evaluators: [
            new ConcreteEvaluator({
                args: ["real", "real"],
                returns: "real",
                primitive: "+"
            })
        ]
    }));
    registerOperator(new OperatorDefinition({
        name: '*',
        args: ["real", "real"],
        returns: "real",
        evaluators: [
            new ConcreteEvaluator({
                args: ["real", "real"],
                returns: "real",
                primitive: "*"
            })
        ]
    }));
    registerOperator(new OperatorDefinition({
        name: '/',
        args: ["real", "real"],
        returns: "real",
        evaluators: [
            new ConcreteEvaluator({
                args: ["real", "real"],
                returns: "real",
                primitive: "/"
            })
        ]
    }));
    registerOperator(new OperatorDefinition({
        name: '-',
        args: ["real", "real"],
        returns: "real",
        evaluators: [
            new ConcreteEvaluator({
                args: ["real", "real"],
                returns: "real",
                primitive: "-"
            })
        ]
    }));
    // Unary minus
    registerOperator(new OperatorDefinition({
        name: '-',
        args: ["real"],
        returns: "real",
        evaluators: [
            new ConcreteEvaluator({
                args: ["real"],
                returns: "real",
                primitive: "-"
            })
        ]
    }));
    // real(real)
    registerOperator(new OperatorDefinition({
        name: 'real',
        args: ["real"],
        returns: "real",
        evaluators: [
            new ConcreteEvaluator({
                args: ["real"],
                returns: "real",
                identity: true
            })
        ]
    }));
    registerOperator(new OperatorDefinition({
        name: '^',
        args: ["real", "real"],
        returns: "real",
        evaluators: [
            new ConcreteEvaluator({
                args: ["real", "real"],
                returns: "real",
                func: Math.pow
            })
        ]
    }));
    registerOperator(new OperatorDefinition({
        name: 'pow',
        args: ["real", "real"],
        returns: "real",
        evaluators: [
            new ConcreteEvaluator({
                args: ["real", "real"],
                returns: "real",
                func: Math.pow
            })
        ]
    }));
    registerOperator(new OperatorDefinition({
        name: 'ln',
        args: ["real"],
        returns: "real",
        evaluators: [
            new ConcreteEvaluator({
                args: ["real"],
                returns: "real",
                func: Math.log
            })
        ]
    }));
    // TODO function aliasing
    registerOperator(new OperatorDefinition({
        name: 'log',
        args: ["real"],
        returns: "real",
        evaluators: [
            new ConcreteEvaluator({
                args: ["real"],
                returns: "real",
                func: Math.log
            })
        ]
    }));
    registerOperator(new OperatorDefinition({
        name: 'sin',
        args: ["real"],
        returns: "real",
        evaluators: [
            new ConcreteEvaluator({
                args: ["real"],
                returns: "real",
                evalType: "new",
                func: Math.sin
            })
        ]
    }));
    registerOperator(new OperatorDefinition({
        name: 'cos',
        args: ["real"],
        returns: "real",
        evaluators: [
            new ConcreteEvaluator({
                args: ["real"],
                returns: "real",
                evalType: "new",
                func: Math.cos
            })
        ]
    }));
    registerOperator(new OperatorDefinition({
        name: 'tan',
        args: ["real"],
        returns: "real",
        evaluators: [
            new ConcreteEvaluator({
                args: ["real"],
                returns: "real",
                evalType: "new",
                func: Math.tan
            })
        ]
    }));
    /**
     * Complex
     */
    // Constructors
    registerOperator(new OperatorDefinition({
        name: 'complex',
        args: ["real"],
        returns: "complex",
        evaluators: [
            new ConcreteEvaluator({
                args: ["real"],
                returns: "complex",
                evalType: "new",
                func: x => new Complex(x, 0)
            }),
            new ConcreteEvaluator({
                args: ["real"],
                returns: "complex",
                evalType: "write",
                func: (x, dst) => {
                    dst.re = x;
                    dst.im = 0;
                }
            })
        ]
    }));
    registerOperator(new OperatorDefinition({
        name: 'complex',
        args: ["real", "real"],
        returns: "complex",
        evaluators: [
            new ConcreteEvaluator({
                args: ["real", "real"],
                returns: "complex",
                evalType: "new",
                func: (x, y) => new Complex(x, y)
            }),
            new ConcreteEvaluator({
                args: ["real", "real"],
                returns: "complex",
                evalType: "write",
                func: (x, y, dst) => {
                    dst.re = x;
                    dst.im = y;
                }
            })
        ]
    }));
    // complex(complex)
    registerOperator(new OperatorDefinition({
        name: 'complex',
        args: ["complex"],
        returns: "complex",
        evaluators: [
            new ConcreteEvaluator({
                args: ["complex"],
                returns: "complex",
                identity: true
            })
        ]
    }));
    registerOperator(new OperatorDefinition({
        name: '+',
        args: ["complex", "complex"],
        returns: "complex",
        evaluators: [
            new ConcreteEvaluator({
                args: ["complex", "complex"],
                returns: "complex",
                evalType: "new",
                func: (x, y) => {
                    return new Complex(x.re + y.re, x.im + y.im);
                }
            }),
            new ConcreteEvaluator({
                args: ["complex", "complex"],
                returns: "complex",
                evalType: "write",
                func: (x, y, dst) => {
                    dst.re = x.re + y.re;
                    dst.im = x.im + y.im;
                }
            })
        ]
    }));
    registerOperator(new OperatorDefinition({
        name: '*',
        args: ["complex", "complex"],
        returns: "complex",
        evaluators: [
            new ConcreteEvaluator({
                args: ["complex", "complex"],
                returns: "complex",
                evalType: "new",
                func: (x, y) => {
                    return new Complex(x.re * y.re - x.im * y.im, x.re * y.im + x.im * y.re);
                }
            }),
            new ConcreteEvaluator({
                args: ["complex", "complex"],
                returns: "complex",
                evalType: "write",
                func: (x, y, dst) => {
                    dst.re = x.re * y.re - x.im * y.im;
                    dst.im = x.re * y.im + x.im * y.re;
                }
            })
        ]
    }));
    registerOperator(new OperatorDefinition({
        name: '/',
        args: ["complex", "complex"],
        returns: "complex",
        evaluators: [
            new ConcreteEvaluator({
                args: ["complex", "complex"],
                returns: "complex",
                evalType: "write",
                func: (z1, z2, dst) => dst.divide(z1, z2)
            })
        ]
    }));
    registerOperator(new OperatorDefinition({
        name: '-',
        args: ["complex", "complex"],
        returns: "complex",
        evaluators: [
            new ConcreteEvaluator({
                args: ["complex", "complex"],
                returns: "complex",
                evalType: "write",
                func: (z1, z2, dst) => dst.subtract(z1, z2)
            })
        ]
    }));
    // Unary minus
    registerOperator(new OperatorDefinition({
        name: '-',
        args: ["complex"],
        returns: "complex",
        evaluators: [
            new ConcreteEvaluator({
                args: ["complex"],
                returns: "complex",
                evalType: "write",
                func: (z, dst) => { dst.re = -z.re; dst.im = -z.im; }
            })
        ]
    }));
    registerOperator(new OperatorDefinition({
        name: '^',
        args: ["complex", "complex"],
        returns: "complex",
        evaluators: [
            new ConcreteEvaluator({
                args: ["complex", "complex"],
                returns: "complex",
                evalType: "write",
                func: (c1, c2, dst) => dst.pow(c1, c2)
            })
        ]
    }));
    /**
     * Real/imag components
     */
    registerOperator(new OperatorDefinition({
        name: 'Re',
        args: ["complex"],
        returns: "real",
        evaluators: [
            new ConcreteEvaluator({
                args: ["complex"],
                returns: "real",
                func: c => c.re
            })
        ]
    }));
    registerOperator(new OperatorDefinition({
        name: 'Im',
        args: ["complex"],
        returns: "real",
        evaluators: [
            new ConcreteEvaluator({
                args: ["complex"],
                returns: "real",
                func: c => c.im
            })
        ]
    }));
    /**
     * Special functions
     */
    registerOperator(new OperatorDefinition({
        name: 'gamma',
        args: ["real"],
        returns: "real",
        evaluators: [
            new ConcreteEvaluator({
                args: ["real"],
                returns: "real",
                func: gammaReal
            })
        ]
    }));
    registerOperator(new OperatorDefinition({
        name: 'gamma',
        args: ["complex"],
        returns: "complex",
        evaluators: [
            new ConcreteEvaluator({
                args: ["complex"],
                returns: "complex",
                evalType: "write",
                func: (z, dst) => dst.gamma(z)
            })
        ]
    }));
    registerOperator(new OperatorDefinition({
        name: 'cos',
        args: ["complex"],
        returns: "complex",
        evaluators: [
            new ConcreteEvaluator({
                args: ["complex"],
                returns: "complex",
                evalType: "write",
                func: (z, dst) => dst.cos(z)
            })
        ]
    }));
    registerOperator(new OperatorDefinition({
        name: 'sin',
        args: ["complex"],
        returns: "complex",
        evaluators: [
            new ConcreteEvaluator({
                args: ["complex"],
                returns: "complex",
                evalType: "write",
                func: (z, dst) => dst.sin(z)
            })
        ]
    }));
    registerOperator(new OperatorDefinition({
        name: 'exp',
        args: ["real"],
        returns: "real",
        evaluators: [
            new ConcreteEvaluator({
                args: ["real"],
                returns: "real",
                func: Math.exp
            })
        ]
    }));
    registerOperator(new OperatorDefinition({
        name: 'exp',
        args: ["complex"],
        returns: "complex",
        evaluators: [
            new ConcreteEvaluator({
                args: ["complex"],
                returns: "complex",
                evalType: "write",
                func: (z, dst) => dst.exp(z)
            })
        ]
    }));

    // Constants are treated as operators, since they are mathematical in nature and may have any number of concrete impls.
    const MathematicalConstants = {
        pi: new OperatorDefinition({
            name: "const_pi",
            args: [],
            returns: "real",
            constant: true,
            evaluators: [
                new ConcreteEvaluator({
                    args: [],
                    returns: "real",
                    func: () => Math.PI,
                    isConstant: true
                })
            ]
        }),
        e: new OperatorDefinition({
            name: "const_e",
            args: [],
            returns: "real",
            constant: true,
            evaluators: [
                new ConcreteEvaluator({
                    args: [],
                    returns: "real",
                    func: () => Math.E,
                    isConstant: true
                })
            ]
        }),
        i: new OperatorDefinition({
            name: "const_i",
            args: [],
            returns: "complex",
            constant: true,
            evaluators: [
                new ConcreteEvaluator({
                    args: [],
                    returns: "complex",
                    func: () => new Complex(0, 1),
                    isConstant: true
                })
            ]
        })
    };

    /**
     * To evaluate a given node whose operators and types have been identified, we provide the following:
     *
     * An evaluation mode, which may be a string;
     * A scope, which is a key-value pair of variable values that will be casted to the corresponding mode type;
     * An option value.
     *
     * Evaluating a constant node:
     *   - Identify the correct concrete type
     *   - Cast the value to the concrete type
     *   - Return
     *
     * Evaluating an ASTGroup:
     *   - Return the result of the first child node
     *
     * Evaluating an OperatorNode:
     *   - Compute the values of all children
     *   - Look up the correct concrete evaluator
     *   - Look up the correct concrete casts
     *   - Cast the values of all children to the correct types
     *   - Call the concrete evaluator. extraArgs is passed as the last parameter if it is not empty.
     *   - Return
     *
     * Evaluating a VariableNode:
     *   - Identify the correct concrete type
     *   - Cast the value in the provided scope to the concrete type
     *   - Return
     */
    class EvaluationError extends Error {
        constructor(message) {
            super(message);
            this.name = 'EvaluationError';
        }
    }
    class ResolutionError extends Error {
        constructor(message) {
            super(message);
            this.name = 'ResolutionError';
        }
    }
    /**
     * Helper function (doesn't need to be fast)
     * @returns {string}
     */
    function prettyPrintNode(node, name, keys, params) {
        let out = [];
        for (let key of keys) {
            let value = node[key];
            if (value != null) {
                if (key === "children") // Get children, pretty printed
                    value = "List{" + value.map(node => node.prettyPrint(params)).join(", ") + "}";
                if (key === "name") // surround with quotes
                    value = `"${value}"`;
                if (key === "type")
                    value = value.toHashStr();
                out.push(`${key}=${value}`);
            }
        }
        return name + "{" + out.join(", ") + "}";
    }
    const KNOWN_KEYS = ["type", "value", "name", "children"];
    const suspiciousVariableNames = [
        "defaultType",
        "throwOnUnresolved"
    ];
    class ASTNode {
        constructor(params) {
            var _a, _b, _c;
            /**
             * MathematicalType of the node (int, complex, etc.). Null if not resolved
             */
            this.type = (_a = params.type) !== null && _a !== void 0 ? _a : null;
            /**
             * Other info about the node (for example, where it was in a parsed string)
             * @type {{}}
             */
            this.info = (_b = params.info) !== null && _b !== void 0 ? _b : {};
            /**
             * The node's operator. If a constant, this will be null and the value will be converted later. If an operator, this
             * must not be null (or the definition is not known). If a variable, this will be called if this is not null.
             * @type {null|OperatorDefinition}
             */
            this.operatorDefinition = (_c = params.operatorDefinition) !== null && _c !== void 0 ? _c : null;
            /**
             * Highest node in this tree
             * @type {null|ASTNode}
             */
            this.topNode = null;
        }
        /**
         * Apply a function f to all children recursively, with some options
         * @param f Function taking in an ASTNode as its first argument, and optionally the depth from the top node as its second
         * @param onlyGroups Only apply f on groups
         * @param childrenFirst Whether to call f on children first
         * @param depth Starting depth
         */
        applyAll(f, onlyGroups = false, childrenFirst = false, depth = 0) {
            if (!onlyGroups)
                f(this, depth);
        }
        /**
         * Whether this node is a group
         */
        isGroup() {
            return false;
        }
        /**
         * Whether this node is an operator node that is semantically a function (and not an infix, prefix or postfix operator)
         */
        isFunctionNode() {
            return false;
        }
        toString() {
            return `[object ${this.nodeTypeAsString()}]`;
        }
        /**
         * Node type as an enum (use nodeTypeAsString() for a string version)
         */
        nodeType() {
            return 0;
        }
        /**
         * Get the node type as a string instead of an enum
         * @returns {string}
         */
        nodeTypeAsString() {
            switch (this.nodeType()) {
                case 0: return "ASTNode";
                case 1: return "ConstantNode";
                case 2: return "VariableNode";
                case 3: return "OperatorNode";
                case 4: return "ASTGroup";
            }
            return "UnknownNode";
        }
        /**
         * For debug use. Example: OperatorNode{type=int, name="+", children=List{ConstantNode{type=int, value="3"}, VariableNode{type=int, name="x"}}}
         */
        prettyPrint(params = {}) {
            return prettyPrintNode(this, this.nodeTypeAsString(), KNOWN_KEYS, params);
        }
        /**
         * Deep clone this ASTNode TODO
         */
        clone() {
            return new ASTNode(this);
        }
        /**
         * Figure out the type of each node, given the type of each variable node within it.
         * Perf: on "x^2+y^2+e^-x^2+pow(3,gamma(2401 + complex(2,3)))", took 0.002 ms / iteration as of Mar 14, 2022
         * @param vars {{}} Mapping from variable names to their types
         * @param opts
         */
        resolveTypes(vars, opts = {}) {
            // Convert all arg values to mathematical types
            let { defaultType = "real", throwOnUnresolved = true } = opts;
            for (let sus of suspiciousVariableNames) {
                if (sus in vars) {
                    localWarn(`Option ${sus} found in first argument to resolveTypes(vars, opts). Note that vars is a dictionary of variables, so ${sus} will be treated as a variable.`, `unusual variable name in resolveTypes()`, 3);
                }
            }
            let revisedVars = {};
            for (let v in vars) {
                if (!vars.hasOwnProperty(v))
                    continue;
                let n = revisedVars[v];
                let mType = toMathematicalType(n);
                if (!mType) {
                    throw new ResolutionError(`Invalid mathematical type ${n} for variable ${v}`);
                }
                revisedVars[v] = mType;
            }
            let revisedType = toMathematicalType(defaultType, true);
            this.applyAll(node => node._resolveTypes({
                vars: revisedVars, throwOnUnresolved, defaultType: revisedType
            }), false /* only groups */, true /* children first */);
            return this;
        }
        /**
         * Whether all operator definitions and types have been resolved for this expression
         * @returns {boolean}
         */
        allResolved() {
            return !!(this.type);
        }
        _resolveTypes(opts) {
        }
        /**
         * Evaluate the function in place (without compilation), passing variable values as a dictionary. This is SLOW. Don't
         * call it unless you're only going to evaluate the function a handful of times
         * @param vars
         * @param opts
         */
        evaluate(vars, { mode = "normal", typecheck = true } = {}) {
            if (!this.allResolved())
                throw new EvaluationError(`This node has not had its types fully resolved (call .resolveTypes())`);
            let convertedMode = toEvaluationMode(mode !== null && mode !== void 0 ? mode : "normal", true); // throws on fail
            return this._evaluate(vars, convertedMode, { mode, typecheck });
        }
        _evaluate(vars, mode, opts) {
            throw new EvaluationError("ASTNode cannot be evaluated directly");
        }
        /**
         * Returns a Map of variable names to information about those variables.
         */
        getVariableDependencies() {
            let knownVars = new Map();
            this.applyAll((node) => {
                if (node.nodeType() === ASTNode.TYPES.VariableNode) {
                    let name = node.name;
                    let info = knownVars.get(name);
                    if (!info) {
                        if (node.type == null) {
                            throw new ResolutionError(`Type of variable ${name} has not been resolved`);
                        }
                        info = {
                            type: node.type,
                            operatorDefinition: node.operatorDefinition,
                            count: 1
                        };
                        knownVars.set(name, info);
                    }
                    else {
                        info.count++;
                    }
                }
            });
            return knownVars;
        }
    }
    // Enum of node types
    ASTNode.TYPES = Object.freeze({
        ASTNode: 0,
        ConstantNode: 1,
        VariableNode: 2,
        OperatorNode: 3,
        ASTGroup: 4
    });
    // Node with children. A plain ASTGroup is usually just a parenthesized thing
    class ASTGroup extends ASTNode {
        constructor(params = {}) {
            var _a;
            super(params);
            this.children = (_a = params.children) !== null && _a !== void 0 ? _a : [];
            this.info.isFunction = false;
        }
        applyAll(func, onlyGroups = false, childrenFirst = false, depth = 0) {
            if (!childrenFirst)
                func(this, depth);
            //let stack: Array<ASTNode> = [ this ]
            let children = this.children;
            for (let i = 0; i < children.length; ++i) {
                let child = children[i];
                if (!onlyGroups || child.isGroup()) {
                    child.applyAll(func, onlyGroups, childrenFirst, depth + 1);
                }
            }
            if (childrenFirst)
                func(this, depth);
            return this;
        }
        isGroup() {
            return true;
        }
        nodeType() {
            return 4;
        }
        clone() {
            return new ASTGroup(this);
        }
        _resolveTypes(opts) {
            // Only called on a raw group, aka a parenthesized group
            let children = this.children;
            for (let i = 0; i < children.length; ++i) {
                children[i]._resolveTypes(opts);
            }
            this.type = children[0].type;
        }
        _evaluate(vars, mode, opts) {
            return this.children[0]._evaluate(vars, mode, opts);
        }
    }
    class ConstantNode extends ASTNode {
        constructor(params) {
            super(params);
            // Generally a text rendition of the constant node; e.g., "0.3" or "50"
            this.value = params.value;
            if (!params.type) {
                throw new EvaluationError(`ConstantNode must be constructed with a mathematical type (e.g., real)`);
            }
            this.type = params.type;
        }
        nodeType() {
            return 1;
        }
        clone() {
            return new ConstantNode(this);
        }
        _resolveTypes(args) {
        }
        _evaluate(vars, mode, opts) {
            if (!this.type) {
                throw new EvaluationError(`Type of constant variable with value ${this.value} has not been resolved`);
            }
            let type = mode.getConcreteType(this.type);
            if (!type) {
                throw new EvaluationError(`Cannot find concrete type in mode ${mode.name} for mathematical type ${this.type.toHashStr()}`);
            }
            return type.castPermissive(this.value);
        }
    }
    class VariableNode extends ASTNode {
        constructor(params) {
            var _a;
            super(params);
            this.name = params.name;
            this.operatorDefinition = (_a = params.operatorDefinition) !== null && _a !== void 0 ? _a : null;
            // TODO var name check in parse string
        }
        nodeType() {
            return 2;
        }
        clone() {
            return new VariableNode(this);
        }
        _resolveTypes(opts) {
            let { vars, defaultType } = opts;
            let name = this.name;
            let info;
            if (vars)
                info = vars[this.name];
            if (!info && name in MathematicalConstants) { // pi, e, i
                let constant = MathematicalConstants[name]; // OperatorDefinition
                info = constant.returns;
                this.operatorDefinition = constant;
            }
            this.type = toMathematicalType(info !== null && info !== void 0 ? info : (defaultType !== null && defaultType !== void 0 ? defaultType : "real"));
        }
        _evaluate(vars, mode, opts) {
            if (this.operatorDefinition) { // pi, e, i
                let evaluator = this.operatorDefinition.getDefaultEvaluator(mode);
                if (evaluator === null) {
                    throw new EvaluationError(`No known definition for constant ${this.name} in mode ${mode.name}`);
                }
                return evaluator.callNew([]);
            }
            let v = vars[this.name];
            if (v === undefined) {
                throw new EvaluationError(`Variable ${this.name} is not defined in the current scope`);
            }
            if (opts.typecheck) {
                let concreteType = mode.getConcreteType(this.type);
                let works = concreteType.typecheck(v);
                if (!works) {
                    let msg = concreteType.typecheckVerbose(v);
                    throw new EvaluationError(`Variable ${this.name} should have concrete type ${concreteType.toHashStr()}. ${msg}`);
                }
            }
            return v;
        }
    }
    class OperatorNode extends ASTGroup {
        constructor(params) {
            var _a, _b, _c;
            super(params);
            this.name = params.name;
            this.children = (_a = params.children) !== null && _a !== void 0 ? _a : [];
            this.extraArgs = (_b = params.extraArgs) !== null && _b !== void 0 ? _b : {};
            this.casts = (_c = params.casts) !== null && _c !== void 0 ? _c : [];
        }
        nodeType() {
            return 3;
        }
        isFunctionNode() {
            return !!this.info.isFunction;
        }
        clone() {
            return new OperatorNode(this);
        }
        childArgTypes() {
            return this.children.map(c => c.type);
        }
        _resolveTypes(opts) {
            let childArgTypes = this.childArgTypes();
            if (!childArgTypes.some(t => t === null)) { // null check done
                let [definition, casts] = resolveOperatorDefinition(this.name, childArgTypes);
                if (definition !== null && casts.every(cast => cast !== null)) {
                    this.type = definition.returns;
                    this.operatorDefinition = definition;
                    this.casts = casts;
                    return;
                }
            }
            this.type = null;
            this.operatorDefinition = null;
            this.casts = null;
            if (opts.throwOnUnresolved) {
                throw new ResolutionError(`Unable to resolve operator definition ${this.name}(${childArgTypes.map(t => { var _a; return (_a = t === null || t === void 0 ? void 0 : t.toHashStr()) !== null && _a !== void 0 ? _a : "unknown"; })})`);
            }
        }
        _evaluate(vars, mode, opts) {
            if (!this.operatorDefinition)
                throw new EvaluationError("Operator definition not resolved");
            if (!this.casts)
                throw new EvaluationError("Casts not resolved");
            let casts = this.casts;
            let childrenValues = this.children.map((c, i) => {
                let cast = casts[i];
                let ccast = cast.getDefaultEvaluator(mode);
                if (ccast === null) {
                    throw new EvaluationError(`No concrete cast (in mode ${mode.name}) from source ${mode.getConcreteType(cast.srcType()).toHashStr()}`
                        + ` to destination ${mode.getConcreteType(cast.dstType()).toHashStr()}`);
                }
                let uncastedChild = c._evaluate(vars, mode, opts);
                return ccast.callNew([
                    uncastedChild
                ]);
            });
            let evaluator = this.operatorDefinition.getDefaultEvaluator(mode);
            if (!evaluator) {
                throw new EvaluationError(`No evaluator (in mode ${mode.name}} for operator ${this.operatorDefinition.prettyPrint()}`);
            }
            return evaluator.callNew(childrenValues);
        }
    }

    /**
     * In this file, we convert strings representing expressions in Grapheme into their ASTNode counterparts. For example,
     * x^2 is compiled to OperatorNode{operator=^, children=[VariableNode{name="x"}, ConstantNode{value="2"}]}
     */
    class ParserError extends Error {
        constructor(message) {
            super(message);
            this.name = 'ParserError';
        }
    }
    // ASTNode that might still have tokens in it
    class UnprocessedASTNode {
        constructor(nodeType, info, children) {
            this.nodeType = nodeType;
            this.info = info;
            this.children = children;
            this.type = "node";
        }
        applyAll(f, childrenFirst = false) {
            let children = this.children;
            if (!childrenFirst)
                f(this);
            for (let i = 0; i < children.length; ++i) {
                let child = children[i];
                if (child instanceof UnprocessedASTNode) {
                    child.applyAll(f, childrenFirst);
                }
                else {
                    f(child);
                }
            }
            if (childrenFirst)
                f(this);
        }
    }
    const operator_regex = /^[*\-\/+^]|^[<>]=?|^[=!]=|^and\s+|^or\s+/;
    const function_regex = /^([a-zA-Z_][a-zA-Z0-9_]*)\(/; // functions may only use (, [ is reserved for indexing
    const constant_regex = /^[0-9]*\.?[0-9]*e?[0-9]+/;
    const variable_regex = /^[a-zA-Z_][a-zA-Z0-9_]*/;
    const paren_regex = /^[()\[\]]/;
    const comma_regex = /^,/;
    const string_regex = /^"(?:[^"\\]|\\.)*"/;
    /**
     * Helper function to throw an error at a specific index in a string.
     * TODO make signature uniform
     * @param string Erroneous parsed string
     * @param info The token in the string where the error occurred, ideally with an index attribute
     * @param message The raw error message, to be combined with contextual information
     * @param noIndex If true, provide no index
     */
    function raiseParserError(string, info, message = "", noIndex = false) {
        var _a;
        let index = -1, token = null, endToken = null;
        if (info !== null) {
            if ('index' in info) {
                index = info.index;
            }
            else {
                token = info.token;
                if (info.endToken != null) {
                    endToken = info.endToken;
                }
                index = token.index;
            }
        }
        if (!noIndex) // can't use an index if we have no index information
            noIndex = index === -1;
        let spaces = noIndex ? '' : ' '.repeat(index);
        let errorLen = ((_a = endToken === null || endToken === void 0 ? void 0 : endToken.index) !== null && _a !== void 0 ? _a : index) - index + 1;
        throw new ParserError('Malformed expression; ' + message + (noIndex ? '' : ' at index ' + index + ':\n' + string + '\n' + spaces + '^'.repeat(errorLen)));
    }
    function raiseUnknownParserError() {
        throw new ParserError("?"); // hi
    }
    function checkParensBalanced(s) {
        // TODO: Handle strings as tokens
        const parenStack = [];
        let i = 0;
        let err = false;
        outer: for (; i < s.length; ++i) {
            let chr = s[i];
            switch (chr) {
                case '(':
                case '[':
                    parenStack.push(chr);
                    break;
                case ')':
                case ']':
                    if (parenStack.length === 0) {
                        err = true;
                        break outer;
                    }
                    if (chr === ')') {
                        let pop = parenStack.pop();
                        if (pop !== '(') {
                            err = true;
                            break outer;
                        }
                    }
                    else {
                        let pop = parenStack.pop();
                        if (pop !== '[') {
                            err = true;
                            break outer;
                        }
                    }
            }
        }
        if (parenStack.length !== 0)
            err = true;
        if (err)
            raiseParserError(s, { index: i }, 'unbalanced parentheses/brackets');
    }
    // Exclude valid variables if needed later
    function isValidVariableName(str) {
        return true;
    }
    const trimRight = ('trimRight' in String.prototype) ? (s) => s.trimRight() : (s) => {
        return s.replace(/\s+$/, '');
    };
    function getTokens(s) {
        let i = 0;
        let original_string = s;
        s = trimRight(s);
        let prev_len = s.length;
        let tokens = [];
        while (s) {
            s = s.trim(); // repeatedly trim off whitespace and grab the next token TODO: optimize to simply iterate
            i += prev_len - s.length;
            prev_len = s.length;
            let match;
            do {
                match = s.match(paren_regex);
                if (match) {
                    tokens.push({
                        type: 'paren',
                        paren: match[0],
                        index: i
                    });
                    break;
                }
                match = s.match(constant_regex);
                if (match) {
                    tokens.push({
                        type: 'constant',
                        value: match[0],
                        index: i
                    });
                    break;
                }
                match = s.match(operator_regex);
                if (match) {
                    tokens.push({
                        type: 'operator',
                        op: match[0].replace(/\s+/g, ''),
                        index: i
                    });
                    break;
                }
                match = s.match(comma_regex);
                if (match) {
                    tokens.push({
                        type: 'comma',
                        index: i
                    });
                    break;
                }
                match = s.match(function_regex);
                if (match) {
                    // First group is the function name, second group is the type of parenthesis (bracket or open)
                    tokens.push({
                        type: 'function',
                        name: match[1],
                        index: i
                    });
                    tokens.push({
                        type: 'paren',
                        paren: '(',
                        index: i + match[1].length
                    });
                    break;
                }
                match = s.match(variable_regex);
                if (match) {
                    tokens.push({
                        type: 'variable',
                        name: match[0],
                        index: i
                    });
                    break;
                }
                match = s.match(string_regex);
                if (match) {
                    tokens.push({
                        type: 'string',
                        contents: match[0].slice(1, -1),
                        index: i
                    });
                    // fall through
                }
                raiseParserError(original_string, { index: i }, 'unrecognized token');
            } while (false);
            let len = match[0].length;
            s = s.slice(len); // rm token
        }
        return tokens;
    }
    function checkValid(tokens, string) {
        if (tokens.length === 0) {
            raiseParserError(string, { index: 0 }, 'empty expression', true);
        }
        for (let i = 0; i < tokens.length - 1; ++i) {
            let token1 = tokens[i];
            let token2 = tokens[i + 1];
            let token2IsUnary = token2.op === '-' || token2.op === '+';
            if ((token1.type === 'operator' || token1.type === 'comma') &&
                (token2.type === 'operator' || token2.type === 'comma') &&
                (!token2IsUnary || i === tokens.length - 2)) {
                raiseParserError(string, token2, 'two consecutive operators');
            }
            if (token1.paren === '(' && token2.paren === ')')
                raiseParserError(string, token2, 'empty parentheses not associated with function call');
            if (token1.paren === '[' && token2.paren === ']')
                raiseParserError(string, token2, 'empty brackets not associated with function call');
            if (token1.type === 'operator' && token2.paren === ')')
                raiseParserError(string, token2, 'operator followed by closing parenthesis');
            if (token1.type === 'operator' && token2.paren === ']')
                raiseParserError(string, token2, 'operator followed by closing bracket');
            if (token1.type === 'comma' && token2.paren === ')')
                raiseParserError(string, token2, 'comma followed by closing parenthesis');
            if (token1.type === 'comma' && token2.paren === ']')
                raiseParserError(string, token2, 'comma followed by closing bracket');
            if (token1.paren === '(' && token2.type === 'comma')
                raiseParserError(string, token2, 'comma after open parenthesis');
            if (token1.paren === '[' && token2.type === 'comma')
                raiseParserError(string, token2, 'comma after starting bracket');
            if (token1.paren === '(' && token2.type === 'operator' && !token2IsUnary)
                raiseParserError(string, token2, 'operator after starting parenthesis');
            if (token1.paren === '[' && token2.type === 'operator' && !token2IsUnary)
                raiseParserError(string, token2, 'operator after starting bracket');
        }
        if (tokens[0].type === 'comma' ||
            (tokens[0].type === 'operator' &&
                !(tokens[0].op === '-' || tokens[0].op === '+')))
            raiseParserError(string, { index: 0 }, 'expression begins with comma or operator');
        const lastToken = tokens[tokens.length - 1];
        if (lastToken.type === 'comma' || lastToken.type === 'operator')
            raiseParserError(string, lastToken, 'expression ends with comma or operator');
    }
    /**
     * Find a pair of parentheses in a list of tokens, namely the first one as indexed by the closing paren/bracket. For
     * example, in (x(y(z)(w))) it will find (z), returning [ paren1 index, paren2 index, paren1 token, paren2 token ]
     */
    function findParenIndices(children) {
        let startIndex = -1;
        let startToken = null;
        for (let i = 0; i < children.length; ++i) {
            let child = children[i];
            if (!('paren' in child))
                continue;
            if (child.paren === '(' || child.paren === '[') {
                startIndex = i;
                startToken = child;
            }
            if ((child.paren === ')' || child.paren === ']') && startIndex !== -1)
                return [startIndex, i, startToken, child];
        }
        return null;
    }
    /**
     * Given a string like "1.5", "3e10", etc., determine whether it is an integer without evaluating it. Assumes the string
     * is well-formed.
     */
    function isStringInteger(s) {
        if (s[0] === '-')
            s = s.slice(1); // trim leading '-'
        let exponent = 0, mIntTrailingZeros = Infinity, mFracLen = 0;
        let e = s.indexOf('e');
        // If mFracLen = 0 (no fractional part) and mIntTrailingZeros = 0 (no integer part), the result is 0, so integer
        // If mFracLen > 0 (fractional part), integer if exponent >= mFracLen
        // If mFracLen = 0 (no fractional part), integer if exponent >= -mIntTrailingZeros
        if (e !== -1) { // get exponent
            exponent = parseInt(s.slice(e + 1), 10);
            if (Number.isNaN(exponent))
                throw new Error("unrecognized exponent " + s.slice(e + 1));
        }
        else {
            e = s.length;
        }
        let p = s.indexOf('.');
        if (p !== -1) {
            for (let i = e - 1; i > p; --i) {
                // find trailing zeros
                if (s[i] !== '0') {
                    mFracLen = i - p;
                    break;
                }
            }
        }
        else {
            p = e;
        }
        for (let i = p - 1; i >= 0; --i) {
            if (s[i] !== '0') {
                mIntTrailingZeros = p - i;
            }
        }
        if (mFracLen !== 0) {
            return exponent >= mFracLen;
        }
        else {
            if (mIntTrailingZeros !== 0) {
                return exponent > -mIntTrailingZeros;
            }
            else {
                // 0
                return true;
            }
        }
    }
    /**
     * Convert constants and variables to their ASTNode counterparts
     */
    function processConstantsAndVariables(tokens) {
        for (let i = 0; i < tokens.length; ++i) {
            let token = tokens[i];
            let node;
            if ('type' in token) {
                switch (token.type) {
                    case 'constant':
                        let type = toMathematicalType(isStringInteger(token.value) ? 'int' : 'real', true /* throw on error */);
                        node = new UnprocessedASTNode("constant", { value: token.value, token, startToken: token, endToken: token, type }, []);
                        break;
                    case 'variable':
                        node = new UnprocessedASTNode("variable", { name: token.name, token, startToken: token, endToken: token }, []);
                        break;
                    default:
                        continue;
                }
                tokens[i] = node;
            }
        }
    }
    // To process parentheses, we find pairs of them and combine them into ASTNodes containing the nodes and
    // tokens between them. We already know the parentheses are balanced, which is a huge help here. We basically go
    // through each node recursively and convert all paren pairs to a node, then recurse into those new nodes
    function processParentheses(rootNode) {
        rootNode.applyAll(node => {
            if (!(node instanceof UnprocessedASTNode))
                return;
            let parensRemaining = true;
            while (parensRemaining) {
                parensRemaining = false;
                let indices = findParenIndices(node.children);
                if (indices) {
                    parensRemaining = true;
                    let [startIndex, endIndex, startToken, endToken] = indices;
                    let newNode = new UnprocessedASTNode("group", {}, []);
                    let expr = node.children.splice(startIndex, endIndex - startIndex + 1, newNode);
                    newNode.children = expr.slice(1, expr.length - 1);
                }
            }
        }, true);
    }
    // Turn function tokens followed by ASTNodes into OperatorNodes
    function processFunctions(rootNode) {
        rootNode.applyAll(node => {
            if (!(node instanceof UnprocessedASTNode))
                return;
            let children = node.children;
            for (let i = 0; i < children.length; ++i) {
                let token = children[i];
                if (!(token instanceof UnprocessedASTNode) && token.type === 'function') {
                    let nextNode = children[i + 1];
                    if (!nextNode || !(nextNode instanceof UnprocessedASTNode)) {
                        raiseUnknownParserError();
                    }
                    let newNode = new UnprocessedASTNode("operator", { name: token.name, isFunction: true }, nextNode.children);
                    children[i] = newNode;
                    // Take children from the node coming immediately after
                    newNode.children = nextNode.children;
                    // Remove the node immediately after
                    children.splice(i + 1, 1);
                }
            }
        }, true);
    }
    // Given a node and an index i of a binary operator, combine the nodes immediately to the left and right of the node
    // into a single binary operator
    function combineBinaryOperator(node, i) {
        const children = node.children;
        let child = children[i];
        if (child instanceof UnprocessedASTNode || !('op' in child)) {
            raiseUnknownParserError();
        }
        let prevChild = children[i - 1];
        let nextChild = children[i + 1];
        if (!(prevChild instanceof UnprocessedASTNode) || !(nextChild instanceof UnprocessedASTNode)) {
            raiseUnknownParserError();
        }
        let newNode = new UnprocessedASTNode("operator", { name: child.op }, [prevChild, nextChild]);
        children.splice(i - 1, 3, newNode);
    }
    // Process the highest precedence operators. Note that e^x^2 = (e^x)^2 and e^-x^2 = e^(-x^2).
    function processUnaryAndExponentiation(root) {
        root.applyAll(node => {
            if (!(node instanceof UnprocessedASTNode))
                return;
            let children = node.children;
            // We iterate backwards
            for (let i = children.length - 1; i >= 0; --i) {
                let child = children[i];
                if (child instanceof UnprocessedASTNode || !('op' in child))
                    continue;
                if (child.op === '-' || child.op === '+') {
                    // If the preceding token is an unprocessed non-operator token, or node, then it's a binary expression
                    let preceding = children[i - 1];
                    if (i !== 0 && ('type' in preceding) && (preceding.type !== 'operator'))
                        continue;
                    let newNode = new UnprocessedASTNode("operator", { name: child.op }, [children[i + 1]]);
                    children.splice(i, 2, newNode);
                }
                else if (child.op === '^') {
                    combineBinaryOperator(node, i);
                    --i;
                }
            }
        }, true);
    }
    // Combine binary operators, going from left to right, with equal precedence for all
    function processOperators(root, operators) {
        root.applyAll(node => {
            if (!(node instanceof UnprocessedASTNode))
                return;
            let children = node.children;
            for (let i = 0; i < children.length; ++i) {
                let child = children[i];
                if (child instanceof UnprocessedASTNode || !('op' in child))
                    continue;
                if (operators.includes(child.op)) {
                    combineBinaryOperator(node, i);
                    --i;
                }
            }
        }, true);
    }
    // The index of each operator is also an enum, which is used in comparison chains to describe which operator is being used
    const comparisonOperators = ['<', '<=', '==', '!=', '>=', '>'];
    // Process "comparison chains", which are sequences of the form 0 <= x < 2. Internally these are transformed into
    // "comparison_chain" operators, which have the form comparison_chain(0, 1 (enum comparison), x, 0 (enum comparison), 2). Gross, but
    // it's hard to cleanly represent these comparison chains otherwise. You *could* represent them using boolean operations,
    // but that duplicates the internal nodes which is inefficient
    function processComparisonChains(root) {
        root.applyAll(node => {
            // TODO: process backwards for efficiency
            if (!(node instanceof UnprocessedASTNode))
                return;
            const children = node.children;
            for (let i = 0; i < children.length; ++i) {
                let child = children[i];
                if (child instanceof UnprocessedASTNode || !('op' in child))
                    continue;
                if (comparisonOperators.includes(child.op)) {
                    let comparisonChainFound = false;
                    // Found a comparison operator token; we now check for whether the tokens +2, +4, etc. ahead of it are also
                    // comparison tokens. If so, we emit a comparison chain
                    // Index of the last comparison token, plus 2
                    let j = i + 2;
                    for (; j < children.length; j += 2) {
                        let nextChild = children[j];
                        if (nextChild instanceof UnprocessedASTNode || !('op' in nextChild))
                            continue;
                        if (comparisonOperators.includes(nextChild.op)) {
                            comparisonChainFound = true;
                        }
                        else {
                            break;
                        }
                    }
                    if (comparisonChainFound) {
                        // The nodes i, i+2, i+4, ..., j-4, j-2 are all comparison nodes. Thus, all nodes in the range i-1 ... j-1
                        // should be included in the comparison chain
                        let comparisonChain = new UnprocessedASTNode("operator", { name: 'comparison_chain' }, []);
                        // Looks something like [ ASTNode, '<', ASTNode, '<=', ASTNode ]
                        let removedChildren = children.splice(i - 1, j - i + 1, comparisonChain // inserts the comparison chain as replacement
                        );
                        // [ ASTNode, ASTNode, ASTNode ]
                        let cchainChildren = (comparisonChain.children = []);
                        let comparisons = []; // [ '<', '<=' ]
                        for (let i = 1; i < removedChildren.length - 2; i += 2) {
                            let child = removedChildren[i];
                            if (!('op' in child))
                                raiseUnknownParserError();
                            comparisons.push(child.op);
                        }
                        for (let i = 0; i < removedChildren.length; i += 2) {
                            cchainChildren.push(removedChildren[i]);
                        }
                        comparisonChain.info.comparisons = comparisons;
                        return;
                    }
                }
            }
        }, true);
    }
    // Remove residual commas from the node
    function removeCommas(root) {
        root.applyAll(node => {
            if (!(node instanceof UnprocessedASTNode))
                return;
            let children = node.children;
            let i = children.length;
            while (i--) {
                if (children[i].type === 'comma')
                    children.splice(i, 1);
            }
        }, true);
    }
    function verifyCommaSeparation(root, string) {
        root.applyAll(node => {
            var _a, _b, _c, _d;
            // Every function with multiple elements in it should have commas separating each argument, with no leading or
            // trailing commas.
            if (!(node instanceof UnprocessedASTNode))
                return;
            let children = node.children;
            let isPlainGroup = node.nodeType === "group";
            let isFunction = !!node.info.isFunction;
            if (!isFunction && !isPlainGroup)
                return;
            if (children.length === 0) {
                // will eventually be fine. () is the empty tuple
                raiseParserError(string, { index: (_b = (_a = node.info.token) === null || _a === void 0 ? void 0 : _a.index) !== null && _b !== void 0 ? _b : -1 }, "empty parentheses");
                return;
            }
            if (children[0].type === 'comma')
                raiseParserError(string, children[0], "leading comma in expression");
            // Must have the form "a,b,c"
            let prevChild = null;
            for (let i = 0; i < children.length; ++i) {
                let child = children[i];
                if ((!prevChild || prevChild.type === "comma") && child.type === "comma") {
                    // child is a token
                    raiseParserError(string, child, "spurious comma");
                }
                if (prevChild && prevChild.type !== "comma" && child.type !== "comma") {
                    // child must be a node
                    if (child instanceof UnprocessedASTNode) {
                        raiseParserError(string, { index: (_d = (_c = child.info.token) === null || _c === void 0 ? void 0 : _c.index) !== null && _d !== void 0 ? _d : -1 }, "trailing expression");
                    }
                    raiseUnknownParserError();
                }
                prevChild = child;
            }
            // TODO tuples
            if (isPlainGroup) {
                if (children.length > 1)
                    raiseParserError(string, { index: -1 }, "tuples not yet implemented");
            }
        }, true);
    }
    /**
     * Attach start and end tokens for each group
     * @param root
     */
    function attachInformation(root) {
        root.applyAll(node => {
            if (!(node instanceof UnprocessedASTNode))
                return;
            let info = node.info;
            if (!info.token) {
                let firstChild = node.children[0];
                let lastChild = node.children[node.children.length - 1];
                if (!(firstChild instanceof UnprocessedASTNode) || !(lastChild instanceof UnprocessedASTNode)) {
                    raiseUnknownParserError();
                }
                info.token = info.startToken = firstChild.info.token;
                info.endToken = lastChild.info.token;
            }
        }, true /* children first */);
    }
    /**
     * Parse a given list of tokens, returning a single ASTNode.
     * Perf: parseString("x^2+y^2+e^-x^2+pow(3,gamma(2401 + complex(2,3)))" took 0.028 ms / iteration as of Mar 14, 2022.
     * @param tokens
     * @param string String where tokens ultimately came from (used for descriptive error messages)
     */
    function parseTokens(tokens, string) {
        // This is somewhat of a recursive descent parser because the grammar is nontrivial, but really isn't that
        // crazy. At intermediate steps, the node is a tree of both processed nodes and unprocessed tokens—a bit odd, but it
        // works.
        // Placed here because all further nodes will be groups or OperatorNodes
        processConstantsAndVariables(tokens);
        // Everything is done recursively within this root node
        let root = new UnprocessedASTNode("group", {}, tokens);
        processParentheses(root);
        processFunctions(root);
        // Order of operations: unary -/+, ^ (all right to left, together); PEMDAS
        processUnaryAndExponentiation(root);
        processOperators(root, ['*', '/']);
        processOperators(root, ['-', '+']);
        // Comparison chains are expressions of the form x <= y < z ..., which are combined into a single funky node called a
        // "comparison_chain" with arguments x, y, z and extra arguments <=, <
        processComparisonChains(root);
        processOperators(root, comparisonOperators);
        processOperators(root, ['and', 'or']);
        // Adds "debugging tokens", aka where each node starts and ends, and the top node
        attachInformation(root);
        verifyCommaSeparation(root, string);
        // processTuples(root, string)
        removeCommas(root);
        let c = root.children[0];
        if (!c)
            raiseUnknownParserError();
        if (c instanceof UnprocessedASTNode) {
            c.info.parsedFrom = string;
            return c;
        }
        raiseUnknownParserError();
    }
    function convertToASTNode(string, n) {
        let nn, isGroup = false;
        switch (n.nodeType) {
            case "constant":
                nn = new ConstantNode(n.info);
                break;
            case "group":
                nn = new ASTGroup(n.info);
                isGroup = true;
                break;
            case "operator":
                nn = new OperatorNode(n.info);
                isGroup = true;
                break;
            case "variable":
                nn = new VariableNode(n.info);
                break;
            case "generic":
            default:
                raiseUnknownParserError();
        }
        if (isGroup) {
            let children = [];
            for (let i = 0; i < n.children.length; ++i) {
                let child = n.children[i];
                if (!(child instanceof UnprocessedASTNode)) {
                    raiseParserError(string, child /* token */, "unprocessed token");
                }
                else {
                    children.push(convertToASTNode(string, child));
                }
            }
            // nn is an ast group by construction
            nn.children = children;
        }
        return nn;
    }
    function parseString(string) {
        // noinspection ALL
        if (typeof string !== "string") {
            throw new ParserError("parseString expects a string");
        }
        checkParensBalanced(string);
        let tokens = getTokens(string);
        checkValid(tokens, string);
        let parsed = parseTokens(tokens, string);
        return convertToASTNode(string, parsed);
    }

    // An assignment graph is a (mathematical or concrete) abstraction of a set of assignments that would occur in a given
    class AssignmentGraph {
        /**
         * Iterate over input variables—variables which are either static or provided as arguments to the function
         */
        *inputNodes() {
            let entries = this.nodes.entries();
            for (let [name, entry] of entries) {
                if (entry.isInput) {
                    yield [name, entry];
                }
            }
        }
        *nodesInOrder() {
            // Starting at $ret, yield notes from left to right in the order they are needed
            let nodes = this.nodes;
            let enteredNodes = new Set();
            let stack = [this.root]; // last element in stack is the element we will recurse into
            let iters = 0;
            const MAX_ITERS = 10000; // prevent infinite loopage
            while (iters < MAX_ITERS && stack.length !== 0) {
                let name = stack[stack.length - 1];
                let node = nodes.get(name);
                if (!node) {
                    throw new CompilationError(`Could not find node with name ${name}`);
                }
                if (enteredNodes.has(name)) {
                    yield [name, node];
                    stack.pop();
                }
                else {
                    if (node.args) {
                        let args = node.args;
                        for (let i = args.length - 1; i >= 0; --i) {
                            if (!enteredNodes.has(args[i]))
                                stack.push(args[i]); // push children in reverse order, so that we examine the first child first
                        }
                    }
                    enteredNodes.add(name);
                }
                iters++;
            }
            if (iters === MAX_ITERS) {
                throw new CompilationError("Infinite loop in assignment graph");
            }
        }
    }
    class MathematicalAssignmentGraph extends AssignmentGraph {
    }
    class ConcreteAssignmentGraph extends AssignmentGraph {
        optimize(opts = {}) {
            // Crude optimization
        }
    }

    class Assembler {
        constructor() {
            let preamble = this.preamble = new PlainCodeFragment();
            let main = this.main = new PlainCodeFragment();
            let fs = this.fragments = new Map();
            fs.set("main", main);
            fs.set("preamble", preamble); // special code fragment
            this.inputFormat = [];
        }
        add(f, fragmentName = "main") {
            let fragment = (fragmentName === "main") ? this.main : this.fragments.get(fragmentName);
            if (!fragment) {
                throw new CompilationError(`Unknown fragment name ${fragmentName}`);
            }
            fragment.add(f);
        }
        prepareConcreteGraph(cGraph, target) {
            let neededInputs = Array.from(cGraph.inputNodes());
            let inputFormat = target.inputFormat;
            let usesScope = inputFormat.includes("scope");
            let typechecks = target.typechecks;
            for (let [name, node] of neededInputs) {
                if (inputFormat.includes(name)) {
                    // Variable is immediately defined
                    if (typechecks) {
                        let f = new TypecheckFragment();
                        f.name = name;
                        f.concreteType = node.type;
                        // TODO
                    }
                }
                else {
                    if (usesScope) {
                        let f = new VariableDefinitionCodeFragment();
                        f.name = name;
                        f.value = `scope.${name}`;
                        f.verbatim = true;
                        this.add(f);
                    }
                    else {
                        throw new CompilationError(`Can't find variable ${name}`);
                    }
                }
            }
            this.inputFormat = inputFormat;
            // First fragment in main is to get the input variables out of the arguments
            for (let [name, node] of cGraph.nodesInOrder()) {
                // Don't need to do anything if not input; everything taken care of above
                if (!node.isInput) {
                    let ev = node.evaluator;
                    if (ev) {
                        let loc = ev.isConstant ? "preamble" : "main"; // put in preamble if it's a constant
                        let construct = ev.evalType === "new";
                        if (construct || loc === "preamble") {
                            let f = new VariableDefinitionCodeFragment();
                            f.name = name;
                            if (ev.primitive) {
                                f.func = ev.primitive;
                            }
                            else {
                                f.func = ev.func;
                            }
                            f.args = node.args;
                            f.construct = construct;
                            this.add(f, loc);
                        }
                        else {
                            // writes evaluator
                            let f = new InvokeVoidFunctionCodeFragment();
                            f.func = ev.func;
                            f.args = [...node.args, name]; // writes to name
                            let cr = new VariableDefinitionCodeFragment();
                            cr.name = name;
                            cr.args = [];
                            cr.func = node.type.init; // create the variable in the preamble
                            cr.construct = true;
                            this.add(f, "main");
                            this.add(cr, "preamble");
                        }
                    }
                    else {
                        if (node.value !== undefined) { // Constant
                            let n = new VariableDefinitionCodeFragment();
                            n.name = name;
                            n.value = node.value;
                            n.construct = true;
                            this.add(n, "preamble");
                        }
                    }
                }
                if (name === cGraph.root) { // single return statement
                    this.add(`return ${cGraph.root}`);
                }
            }
            let inputCTypes = this.inputFormat.map(varName => {
                if (varName === "scope") {
                    return "scope";
                }
                return cGraph.nodes.get(varName).type;
            });
            this.inputTypes = inputCTypes;
            this.returns = cGraph.nodes.get(cGraph.root).type;
        }
        compile() {
            // Map of closure var names to imported objects to be passed into the closure
            let imports = new Map();
            let exports = new Map();
            let internalFunctions = new Map();
            addInternalFunction([], "preamble");
            addInternalFunction([], "main");
            let writingTo = internalFunctions.get("preamble");
            write("'use strict';");
            function importObject(o) {
                let existing = imports.get(o);
                if (existing !== undefined) {
                    return existing;
                }
                let name = "$import_" + genVariableName();
                imports.set(o, name);
                return name;
            }
            function enterFunction(fName) {
                let intF = internalFunctions.get(fName);
                if (!intF) {
                    throw new CompilationError(`Unrecognized internal function name ${intF}`);
                }
                writingTo = intF;
            }
            function write(s) {
                writingTo.text += s;
            }
            function add(s, fName = "") {
                if (fName) {
                    // TODO undef check
                    internalFunctions.get(fName).text += s;
                }
                else {
                    write(s);
                }
            }
            function addMain(s) {
                add(s, "main");
            }
            function addPreamble(s) {
                add(s, "preamble");
            }
            function addInternalFunction(args, forcedName = "") {
                let name = forcedName || ("$function_" + genVariableName());
                internalFunctions.set(name, {
                    args: [],
                    text: ""
                });
                return name;
            }
            function addExport(exportName, internalName) {
                if (exports.get(exportName)) {
                    throw new CompilationError(`Duplicate export name ${exportName}`);
                }
                exports.set(exportName, internalName);
            }
            function assembleInternalFunction(fName) {
                let f = internalFunctions.get(fName);
                if (!f) {
                    throw new CompilationError(`Failed to compile internal function ${fName} (not found)`);
                }
                let { args, text } = f;
                let argsJoined = args.join(', ');
                return `function ${fName}(${argsJoined}) {\n` + text + "}\n";
            }
            let env = {
                assembler: this,
                importFunction: importObject,
                importValue: importObject,
                add,
                addInternalFunction,
                addMain,
                addPreamble,
                enterMain: () => {
                    enterFunction("main");
                },
                enterPreamble: () => {
                    enterFunction("preamble");
                },
                enterFunction,
                addExport
            };
            enterFunction("main");
            this.main.compileToEnv(env);
            // Preamble compilation should occur after all internal compilations
            enterFunction("preamble");
            this.preamble.compileToEnv(env);
            let mainF = internalFunctions.get("main");
            let preambleF = internalFunctions.get("preamble");
            // The closure has the following structure:
            // function ($import_$1, $import_$2, ...) {
            //   var $2 = $import_$1()
            //   // ... preamble ...
            //   function main (scope, ... input format ...) {
            //      // Function body
            //      return $ret
            //   }
            //   return { evaluate: main }  // exports
            // }
            let importArray = []; // passed arguments to closure
            let importNames = []; // closure argument names
            for (let [o, name] of imports.entries()) {
                importArray.push(o);
                importNames.push(name);
            }
            mainF.args = this.inputFormat;
            preambleF.args = importNames; // unused
            let fsText = "";
            for (let fName of internalFunctions.keys()) {
                if (fName !== "preamble")
                    fsText += assembleInternalFunction(fName);
            }
            addExport("evaluate", "main");
            // Build export text
            let exportText = "return {";
            for (let [name, e] of exports.entries()) {
                exportText += `${name}: ${e},`;
            }
            exportText += "}";
            let fBody = preambleF.text + fsText + exportText;
            //console.log(fBody)
            // Invoke the closure
            let result = (new Function(...importNames, fBody)).apply(null, importArray);
            return {
                result,
                inputFormat: this.inputFormat,
                inputTypes: this.inputTypes,
                returns: this.returns
            };
        }
    }
    class TypecheckFragment {
        compileToEnv(env) {
            let t = this.concreteType;
            let tc = t.typecheck;
            let tcv = t.typecheckVerbose;
            if (!tc) {
                throw new CompilationError(`No defined typecheck for concrete type ${t.toHashStr()}`);
            }
            env.importFunction(tc);
            tcv ? env.importFunction(tcv) : ''; // if no verbose typecheck available, don't include it
            this.name;
            /*env.addMain(`if (${typecheckFast}(${this.name}) {
        
            }`)*/
        }
    }
    // Just a group of code fragments
    class PlainCodeFragment {
        constructor() {
            this.contents = [];
        }
        add(f) {
            this.contents.push(f);
        }
        compileToEnv(env) {
            // Just compile all children
            this.contents.map(c => assembleFragment(env, c));
        }
    }
    class InvokeVoidFunctionCodeFragment {
        compileToEnv(env) {
            let { func, args } = this;
            args = args !== null && args !== void 0 ? args : [];
            let s = args.join(', ');
            env.add(`${env.importFunction(func)}(${s});\n`);
        }
    }
    function assembleFragment(env, f) {
        (typeof f === "string") ? env.add(f) : f.compileToEnv(env);
    }
    class VariableDefinitionCodeFragment {
        compileToEnv(env) {
            let { name, func, args, value, verbatim, construct } = this;
            args = args !== null && args !== void 0 ? args : [];
            let v = "";
            if (typeof func === "function") {
                let s = args.join(', ');
                v = `${env.importFunction(func)}(${s})`;
                // Comma separated, etc.
            }
            else if (typeof func === "string") {
                // Primitive
                let len = args.length;
                if (len === 0 || len > 2) {
                    // TODO specificity
                    throw new CompilationError(`Invalid number of arguments (${len}) to primitive ${func}`);
                }
                v = (len === 1) ? `${func}(${args[0]})` // unary
                    : `(${args[0]}) ${func} (${args[1]})`; // binary
            }
            else {
                v = verbatim ? (value + '') : env.importValue(value);
            }
            env.add(`${construct ? "var " : ""}${name} = ${v};\n`);
        }
    }

    class CompilationError extends Error {
        constructor(message) {
            super(message);
            this.name = 'CompilationError';
        }
    }
    let id = 0;
    /**
     * Generate a unique variable name that will not conflict with other names (locally or globally; no name will ever be
     * returned twice)
     * @returns {string}
     */
    function genVariableName() {
        return "$" + (++id);
    }
    const defaultTarget = {
        mode: "normal",
        inputFormat: "scope",
        typechecks: true,
        returnNew: true
    };
    // -1 if fine, otherwise, index of problematic
    function checkStringArray(o) {
        for (let i = 0; i < o.length; ++i) {
            if (typeof o[i] !== "string") {
                return i;
            }
        }
        return -1;
    }
    // Throws if an input format is invalid (i.e., if there are two arguments with the same name)
    function checkInputFormat(inputFormat) {
        let p = checkStringArray(inputFormat);
        if (p !== -1) {
            throw new CompilationError(`Provided variable name at index ${p} in input format is not a string`);
        }
        for (let i = 0; i < inputFormat.length; ++i) {
            let varName = inputFormat[i];
            for (let j = i + 1; j < inputFormat.length; ++j) {
                if (varName === inputFormat[j]) {
                    throw new CompilationError(`Variable ${varName} is defined twice (at indices ${i} and ${j})`);
                }
            }
        }
    }
    function fillTargetOptions(nodeOpts, opts, rootProperties, index) {
        var _a, _b, _c, _d, _e, _f, _g;
        if (typeof opts !== "object") {
            throw new CompilationError(`Provided target option at index ${index} is not an object`);
        }
        let givenMode = (_a = opts.mode) !== null && _a !== void 0 ? _a : "normal";
        let mode = toEvaluationMode(givenMode, true /* throw on error */);
        let typechecks = (_c = (_b = opts.typechecks) !== null && _b !== void 0 ? _b : nodeOpts.typechecks) !== null && _c !== void 0 ? _c : true;
        let returnNew = (_e = (_d = opts.returnNew) !== null && _d !== void 0 ? _d : nodeOpts.returnNew) !== null && _e !== void 0 ? _e : true;
        let inputFormat = (_f = opts.inputFormat) !== null && _f !== void 0 ? _f : "scope";
        let staticVariables = (_g = nodeOpts.staticVariables) !== null && _g !== void 0 ? _g : [];
        if (typeof inputFormat === "string") {
            inputFormat = [inputFormat];
        }
        inputFormat = inputFormat;
        checkInputFormat(inputFormat);
        if (!Array.isArray(staticVariables)) {
            throw new CompilationError(`Static variables must be an array`);
        }
        let p = checkStringArray(staticVariables);
        if (p !== -1) {
            throw new CompilationError(`Provided variable name at index ${p} in input format is not a string`);
        }
        return {
            mode: mode,
            typechecks,
            returnNew,
            inputFormat,
            staticVariables,
            usedVariables: rootProperties.usedVariables
        };
    }
    function createAssnGraph(root) {
        let graph = new MathematicalAssignmentGraph();
        let assnMap = new Map();
        // ASTNode -> graph node name
        let astToGraphMap = new Map();
        //astToGraphMap.set(root, "$ret")
        function defineGraphNode(name, astNode, inf) {
            if (astNode) {
                astToGraphMap.set(astNode, name);
            }
            assnMap.set(name, inf);
        }
        // Implicitly left to right
        root.applyAll((astNode) => {
            var _a, _b;
            let gNode = null;
            let name = (_a = astToGraphMap.get(astNode)) !== null && _a !== void 0 ? _a : genVariableName();
            switch (astNode.nodeType()) {
                case ASTNode.TYPES.VariableNode: {
                    if (astToGraphMap.get(astNode)) {
                        // Only define variables once
                        return;
                    }
                    if (!astNode.operatorDefinition) {
                        name = astNode.name;
                        gNode = {
                            name,
                            type: astNode.type,
                            isConditional: false,
                            isCast: false,
                            isInput: true,
                            astNode
                        };
                        break;
                    }
                }
                // Fall through
                case ASTNode.TYPES.OperatorNode:
                    let n = astNode;
                    // @ts-ignore
                    let args = (_b = n.children) !== null && _b !== void 0 ? _b : [];
                    // @ts-ignore
                    let casts = (args.length === 0) ? [] : n.casts;
                    let castedArgs = casts.map((cast, i) => {
                        let arg = args[i];
                        let argName = astToGraphMap.get(arg);
                        if (!argName) {
                            throw new CompilationError("?");
                        }
                        if (cast.isIdentity()) {
                            return argName;
                        }
                        let castedArgName = genVariableName();
                        // Create node for the cast
                        defineGraphNode(castedArgName, arg, {
                            name: castedArgName,
                            type: cast.dstType(),
                            isConditional: false,
                            isCast: true,
                            isInput: false,
                            args: [argName],
                            operatorDefinition: cast,
                            astNode: arg // for casts, store the argument as the corresponding node
                        });
                        return castedArgName;
                    });
                    gNode = {
                        name,
                        type: n.type,
                        isConditional: false,
                        isCast: false,
                        isInput: false,
                        args: castedArgs,
                        operatorDefinition: n.operatorDefinition,
                        astNode
                    };
                    break;
                case ASTNode.TYPES.ASTGroup:
                    // Groups are entirely elided by mapping them to the variable name of their only child
                    let c = astNode.children[0];
                    if (!c) {
                        throw new CompilationError("Empty ASTGroup in expression");
                    }
                    astToGraphMap.set(astNode, astToGraphMap.get(c));
                    return;
                case ASTNode.TYPES.ConstantNode:
                    gNode = {
                        name,
                        type: astNode.type,
                        isConditional: false,
                        isCast: false,
                        isInput: false,
                        value: astNode.value,
                        astNode
                    };
                    break;
                case ASTNode.TYPES.ASTNode:
                    throw new CompilationError(`Raw ASTNode in expression`);
            }
            defineGraphNode(name, astNode, gNode);
        }, false /* all children */, true /* children first */);
        graph.nodes = assnMap;
        let graphRoot = astToGraphMap.get(root);
        if (!graphRoot) {
            throw new CompilationError("?");
        }
        graph.root = graphRoot;
        return graph;
    }
    /**
     * Attempt to convert a mathematical assignment graph into a concrete graph
     * @param mGraph Mathematically processed and optimized graph
     * @param target (Filled) compile target
     * @param index Index in the passed target array
     */
    function concretizeAssnGraph(mGraph, target, index) {
        let cNodes = new Map();
        let mode = target.mode;
        function defineGraphNode(name, cNode) {
            cNodes.set(name, cNode);
        }
        function raiseNoConcreteType(mType) {
            throw new CompilationError(`No concrete type found in mode ${mode.toString()} for mathematical type ${mType.toHashStr()}`);
        }
        // Compute concrete types of input nodes first
        for (let [name, mNode] of mGraph.inputNodes()) {
            let mType = mNode.type;
            let cType = mode.getConcreteType(mType);
            if (!cType)
                raiseNoConcreteType(mType);
            defineGraphNode(name, {
                name,
                type: cType,
                isConditional: mNode.isConditional,
                isCast: mNode.isCast,
                isInput: mNode.isInput,
                astNode: mNode.astNode
            });
        }
        for (let [name, mNode] of mGraph.nodesInOrder()) {
            let mType = mNode.type;
            let cType = mode.getConcreteType(mType);
            if (!cType)
                raiseNoConcreteType(mType);
            let o = mNode.operatorDefinition;
            if (o) {
                let argTypes = mNode.args.map(s => {
                    let cNode = cNodes.get(s);
                    if (!cNode)
                        throw new CompilationError("?");
                    return cNode.type;
                });
                let evalType = "writes"; // preferred
                if (name === "$ret") {
                    if (target.returnNew) { // if returning, prefer a new evaluator (for efficiency's sake)
                        evalType = "new";
                    }
                }
                let evaluator = o.findEvaluator(argTypes, { evalType });
                if (!evaluator) {
                    throw new CompilationError(`Unable to find evaluator ${o.prettyPrint()} in mode ${mode.toString()}, accepting concrete types (${argTypes.map(c => c.toHashStr()).join(' ')})`);
                }
                defineGraphNode(name, {
                    name,
                    type: cType,
                    isConditional: mNode.isConditional,
                    isCast: mNode.isCast,
                    isInput: mNode.isInput,
                    evaluator,
                    args: mNode.args,
                    astNode: mNode.astNode
                });
            }
            else {
                defineGraphNode(name, {
                    name,
                    type: cType,
                    isConditional: mNode.isConditional,
                    isCast: mNode.isCast,
                    isInput: mNode.isInput,
                    astNode: mNode.astNode,
                    value: mNode.value
                });
            }
        }
        let graph = new ConcreteAssignmentGraph();
        graph.nodes = cNodes;
        graph.root = mGraph.root;
        return graph;
    }
    /**
     * Compile an ASTNode into an evaluable function or functions. This should be preferred for any expression that will be
     * evaluated many times. This function needs to be pretty well optimized...
     * @param root
     * @param options
     */
    function compileNode(root, options = {}) {
        var _a;
        if (!(root instanceof ASTNode)) {
            if (!(typeof root === "string"))
                throw new CompilationError("First argument to compileNode must be an ASTNode or string");
            root = parseString(root);
        }
        if (!root.allResolved()) {
            root.resolveTypes((_a = options.variables) !== null && _a !== void 0 ? _a : {}, Object.assign(Object.assign({}, options.resolveTypes), { throwOnUnresolved: true }));
        }
        let targetOpts = options.targets;
        if (!targetOpts) {
            targetOpts = defaultTarget;
        }
        if (!Array.isArray(targetOpts)) {
            targetOpts = [targetOpts];
        }
        let rootProperties = {
            usedVariables: root.getVariableDependencies()
        };
        targetOpts = targetOpts;
        // Convert each target to a full target
        let targets = [];
        for (let i = 0; i < targetOpts.length; ++i) {
            let to = targetOpts[i];
            targets.push(fillTargetOptions(options, to, rootProperties, i));
        }
        let mAssignmentGraph = createAssnGraph(root);
        let compiledResults = [];
        for (let i = 0; i < targets.length; ++i) {
            let target = targets[i];
            let cAssignmentGraph = concretizeAssnGraph(mAssignmentGraph, target);
            cAssignmentGraph.optimize();
            // The crude procedure to build a target is now as follows:
            // - Variable retrieval from the input
            // - Typechecks, if desired
            // - Compiled assignment graph
            // - Return $ret
            let assembler = new Assembler();
            assembler.prepareConcreteGraph(cAssignmentGraph, target);
            let compiled = assembler.compile();
            compiledResults.push({
                evaluate: compiled.result.evaluate,
                returns: compiled.returns,
                inputFormat: compiled.inputFormat,
                inputTypes: compiled.inputTypes,
                targetOptions: target
            });
        }
        if (!Array.isArray(targetOpts)) {
            compiledResults = compiledResults[0];
        }
        return {
            targets: compiledResults
        };
    }
    // grep "from [^.]\+'$" -r src

    let intToReal = [
        registerConcreteCast(new ConcreteCast({
            src: "int",
            dst: "real",
            identity: true
        })),
        registerConcreteCast(new ConcreteCast({
            src: "interval_int",
            dst: "interval_real",
            identity: true
        }))
    ];
    let realToComplex = [
        registerConcreteCast(new ConcreteCast({
            src: "real", dst: "complex",
            evalType: "write", func: (src, dst) => { dst.re = src; dst.im = 0; }
        })),
        registerConcreteCast(new ConcreteCast({
            src: "real", dst: "complex",
            evalType: "new", func: r => new Complex(r, 0)
        }))
    ];
    let intToComplex = [
        registerConcreteCast(new ConcreteCast({
            src: "int", dst: "complex",
            evalType: "write", func: (src, dst) => { dst.re = src; dst.im = 0; }
        })),
        registerConcreteCast(new ConcreteCast({
            src: "int", dst: "complex",
            evalType: "new", func: r => new Complex(r, 0)
        }))
    ];
    {
        registerMathematicalCast(new MathematicalCast({
            src: "int",
            dst: "real",
            evaluators: intToReal
        }));
        registerMathematicalCast(new MathematicalCast({
            src: "int",
            dst: "complex",
            evaluators: intToComplex
        }));
        registerMathematicalCast(new MathematicalCast({
            src: "real",
            dst: "complex",
            evaluators: realToComplex
        }));
    }

    function hue2rgb(p, q, t) {
        if (t < 0)
            t += 1;
        else if (t > 1)
            t -= 1;
        if (t < 1 / 6)
            return p + (q - p) * 6 * t;
        if (t < 1 / 2)
            return q;
        if (t < 2 / 3)
            return p + (q - p) * (2 / 3 - t) * 6;
        return p;
    }
    function hslToRGB(h, s, l) {
        let r, g, b;
        if (s === 0) {
            r = g = b = l; // achromatic
        }
        else {
            let q = l < 0.5 ? l * (1 + s) : l + s - l * s;
            let p = 2 * l - q;
            r = hue2rgb(p, q, h + 1 / 3);
            g = hue2rgb(p, q, h);
            b = hue2rgb(p, q, h - 1 / 3);
        }
        return [Math.round(r * 255), Math.round(g * 255), Math.round(b * 255)];
    }
    function complexToRGB(c) {
        let h = (Complex.arg(c) + 2 * Math.PI / 3) / (2 * Math.PI);
        let s = 1;
        let l = 2 / Math.PI * Math.atan(Complex.abs(c));
        return hslToRGB(h, s, l);
    }
    function fastAtan(x) {
        if (x > 1)
            return (Math.PI / 2 - fastAtan(1 / x));
        return (Math.PI / 4) * x - x * (x - 1) * (0.2447 + 0.0663 * x);
    }
    function writeComplexToRGBA(c, arr, index, colorScale) {
        // Somewhat optimized
        let re = c.re, im = c.im;
        const TWO_PI_OVER_3 = 2 * Math.PI / 3;
        const TWO_OVER_PI = 2 / Math.PI;
        let arg = 0; // argument
        if (!Number.isFinite(re) || !Number.isFinite(im)) {
            arg = Math.atan2(im, re);
        }
        else {
            let absX = Math.abs(re), absY = Math.abs(im);
            let a = (absX > absY) ? (absY / absX) : (absX / absY);
            let s = a * a;
            arg = ((-0.0464964749 * s + 0.15931422) * s - 0.327622764) * s * a + a;
            if (absY > absX)
                arg = Math.PI / 2 - arg;
            if (re < 0)
                arg = Math.PI - arg;
            if (im < 0)
                arg = -arg;
        }
        let h = (arg + TWO_PI_OVER_3) * (1 / (2 * Math.PI));
        let l = TWO_OVER_PI * fastAtan(Math.sqrt(re * re + im * im) / colorScale);
        let q = l < 0.5 ? l * 2 : 1;
        let p = 2 * l - q;
        h -= 1 / 3;
        if (h < 0)
            h += 1;
        if (h > 1)
            h -= 1;
        let r = 0, g = 0, b = 0, d = (q - p) * 6;
        // h is between 0 and 1
        if (h < 1 / 6) {
            b = p + d * h;
            g = q;
            r = p;
        }
        else if (h < 1 / 3) {
            b = q;
            g = p + d * (1 / 3 - h);
            r = p;
        }
        else if (h < 1 / 2) {
            b = q;
            g = p;
            r = p + d * (h - 1 / 3);
        }
        else if (h < 2 / 3) {
            b = p + d * (2 / 3 - h);
            g = p;
            r = q;
        }
        else if (h < 5 / 6) {
            b = p;
            g = p + d * (h - 2 / 3);
            r = q;
        }
        else {
            b = p;
            g = q;
            r = p + d * (1 - h);
        }
        arr[index] = (r * 255) | 0;
        arr[index + 1] = (g * 255) | 0;
        arr[index + 2] = (b * 255) | 0;
        arr[index + 3] = 255;
    }

    /**
     * The concept here is to allow the execution of expensive functions both synchronously and asynchronously, without the
     * need for a web worker or other heavyweight techniques. There are benefits to both synchronous and asynchronous
     * execution; some functions are so oft-executed and take such a short time that there is no point to using setTimeout
     * and making it asynchronous. I fear that the proliferation of asynchronous APIs all over the Internet discourages
     * simple, effective code. Also, the current asynchronous APIs aren't the most versatile. For example, how could we
     * track the progress of a render, or cancel the render, via Promises alone?
     *
     * Web workers, while useful (I plan to eventually implement them), are difficult. They can't really do rendering work,
     * and if the function in question takes an absurdly long amount of time to execute, it cannot be terminated gracefully;
     * the entire worker needs to be terminated and then restarted.
     *
     * We use a generator-like object called a "bolus". Why? Because I like that word. Also, it makes it feel
     * like the evaluation of these expensive functions is like digestion. We consume a bolus and digest it asynchronously;
     * it's not like while we're digesting, we can't do anything else. We do get periodic interruptions—stomach cramps,
     * defecation—but it does not control our life. If digestion is taking too long, we can go to the doctor and get a
     * laxative. Using a Web Worker is like giving the bolus to a chemical digester (or another person), and then eating the
     * digested remains; not appetizing, and the process of transferring a disgusting bolus soup is not pleasant. If we find
     * out the bolus is poisonous (aka, we don't want to fully digest it), we can vomit up the bolus, but this is not
     * guaranteed. If this bolus is extremely poisonous, we may die; similarly, if a Grapheme bolus is poorly made, it may
     * still crash the webpage. (Okay, henceforth every "bolus" is a Grapheme bolus.)
     *
     * The bolus may accept any number of arguments. If it is detected to be a normal function (that is, one whose return
     * value does not have a "next" function), its result is given if it's synchronously evaluated, or given as a Promise if
     * asynchronously evaluated. If it is a generator, then during its execution it may periodically yield. If synchronously
     * evaluated, the wrapper will simply keep calling .next() (digestion) until it returns, and then return this value.
     * If asynchronously evaluated, the wrapper will keep calling .next() until some amount of time has elapsed (default is
     * 8 ms, since a frame is 1/60 s) since the function call, or the function returns; in the former case, a timeout will
     * be called to unblock, and in the latter case, the result of the function resolves the Promise.
     *
     * There are additional things that may be given to the wrapper functions for convenience. For example, both sync and
     * asyncEvaluate can be told to throw an error (and thus in the latter case, reject the Promise) if too much time has
     * elapsed. Note that this won't prevent an errant function which enters an infinite loop and NEVER yields from crashing
     * the browser, but in the case of syncEvaluate, it can prevent crashes. Furthermore, asyncEvaluate may be given an
     * additional "onProgress" callback function along with the bolus, which is called based on the estimated time for a
     * bolus to finish, and the Promise it returns is equipped with a special function .cancel() which may be called to
     * terminate the function (and call reject()) before it actually ends. This is useful for things like cancelling
     * expensive updates.
     */
    class BolusTimeoutError extends Error {
        constructor(message) {
            super(message);
            this.name = 'BolusTimeoutError';
        }
    }
    class BolusCancellationError extends Error {
        constructor(message) {
            super(message);
            this.name = 'BolusCancellationError';
        }
    }
    /**
     * Digest a bolus directly, which is ideal for some quickly evaluated boluses. A timeout may also be provided which will
     * terminate the bolus early if necessary. Note that if the bolus finishes and more than timeout ms have elapsed, an
     * error will not be thrown, but if the bolus has yielded without finishing and more than timeout ms have elapsed, it
     * will throw a BolusTimeoutError.
     *
     * Functions may return a bolus or, if they are exceedingly cheap, may return the value. Thus, syncDigest forwards non-
     * boluses directly.
     * @param bolus The bolus to evaluate
     * @param timeout Timeout length in milliseconds (-1 if no timeout)
     */
    function syncDigest(bolus, timeout = -1) {
        var _a, _b;
        // @ts-ignore
        if (bolus == null || typeof (bolus === null || bolus === void 0 ? void 0 : bolus.next) !== 'function') {
            // Forward non-boluses
            return { result: bolus, timeElapsed: 0 };
        }
        bolus = bolus;
        const startTime = Date.now();
        try {
            // Allow timeouts between one ms and one day
            if (timeout >= 1 && timeout <= 8.64e7) {
                /**
                 * Note: this code is not safe for time changes, which perhaps we can fix at some point.
                 * Also, there are some browser features (notably Firefox's privacy.resistFingerprinting) that artificially rounds
                 * the Date.now() and performance.now() values. Indeed, their accuracy is never guaranteed. That is unfortunately
                 * a fundamental limitation.
                 */
                while (true) {
                    // Iterate through the bolus
                    const next = bolus.next();
                    const delta = Date.now() - startTime;
                    if (next.done) {
                        // return the result if done
                        return { result: next.value, timeElapsed: delta };
                    }
                    if (delta > timeout) {
                        // Clean up if needed
                        (_a = bolus.cleanup) === null || _a === void 0 ? void 0 : _a.call(bolus);
                        throw new BolusTimeoutError('Bolus did not digest within ' + timeout + ' ms.');
                    }
                }
            }
            else if (timeout !== -1) {
                throw new RangeError('Invalid timeout, which must be between 1 and 86,400,000 ms, or -1 to signify no timeout.');
            }
            // timeout is -1
            while (true) {
                const next = bolus.next();
                if (next.done)
                    return { result: next.value, timeElapsed: Date.now() - startTime };
            }
        }
        finally {
            // Potentially clean up
            (_b = bolus.cleanup) === null || _b === void 0 ? void 0 : _b.call(bolus);
        }
    }
    class BolusPromise {
        constructor(executor, bolus, st, timeout, timeStep, onProgress, onCleanup, usePostMessage) {
            this._bolus = bolus;
            this._startTime = st;
            this._timeout = timeout;
            this._timeStep = timeStep;
            this._onProgress = onProgress;
            this._onCleanup = onCleanup;
            this._cancelled = false;
            if (!executor) {
                this._executor = this.constructExecutor(usePostMessage);
            }
            else {
                this._executor = executor;
            }
        }
        then() {
            let e = this._executor;
            return e.then.apply(e, arguments);
        }
        catch() {
            let e = this._executor;
            return e.catch.apply(e, arguments);
        }
        constructExecutor(usePostMessage) {
            let id = usePostMessage ? Math.random() : 0;
            let msgData = { asyncBolusId: id }; // posting this object will cause the corresponding bolus to step
            let tm;
            let attachCallback = usePostMessage ? () => {
                window.addEventListener("message", tm);
                window.postMessage(msgData, location.origin);
            } : () => {
                id = setTimeout(tm, 0);
            };
            let scheduleCallback = usePostMessage ? () => {
                window.postMessage(msgData, tm);
            } : () => id = setTimeout(tm, 0);
            let cleanupCallback = usePostMessage ? () => {
                window.removeEventListener("message", tm);
            } : () => clearTimeout(id);
            return new Promise((resolve, reject) => {
                tm = (e) => {
                    var _a, _b, _c, _d;
                    if (((_a = e.data) === null || _a === void 0 ? void 0 : _a.asyncBolusId) !== id)
                        return;
                    try {
                        if (this._cancelled) {
                            throw new BolusCancellationError("Bolus was cancelled");
                        }
                        let stepResult = this._step();
                        if (stepResult.done) {
                            cleanupCallback();
                            resolve({ result: stepResult.value, timeElapsed: Date.now() - this._startTime });
                            (_b = this._onProgress) === null || _b === void 0 ? void 0 : _b.call(this, 1);
                            return;
                        }
                        else {
                            (_c = this._onProgress) === null || _c === void 0 ? void 0 : _c.call(this, stepResult.value);
                        }
                        // timeouts/errors will call reject implicitly
                        // Post message to itself
                        scheduleCallback();
                    }
                    catch (e) {
                        (_d = this._onCleanup) === null || _d === void 0 ? void 0 : _d.call(this);
                        cleanupCallback();
                        reject(e);
                    }
                };
                attachCallback();
            });
        }
        _step() {
            var _a;
            let begin = Date.now();
            let startTime = this._startTime;
            let step = this._timeStep;
            let bolus = this._bolus;
            let timeout = this._timeout;
            while (true) {
                const next = bolus.next();
                if (next.done) {
                    // return the result if done
                    return next;
                }
                const delta = Date.now() - startTime;
                if (timeout !== -1 && delta > timeout) {
                    (_a = bolus.cleanup) === null || _a === void 0 ? void 0 : _a.call(bolus);
                    throw new BolusTimeoutError('Bolus did not digest within ' + timeout + ' ms.');
                }
                const stepDelta = Date.now() - begin;
                if (stepDelta > step) {
                    return next;
                }
            }
        }
        cancel() {
            this._cancelled = true;
            return this;
        }
    }
    /**
     * Digest a bolus asynchronously.
     * @param bolus
     * @param opts
     */
    function asyncDigest(bolus, opts = {}) {
        var _a, _b, _c, _d, _e;
        if (typeof (bolus === null || bolus === void 0 ? void 0 : bolus.next) !== 'function') {
            localWarn("Nonbolus passed to asyncDigest (.next is either not a property or not a function", "asyncDigest nonbolus", 1);
            // Forward non-boluses
            return new BolusPromise(Promise.resolve({ result: bolus, timeElapsed: 0 }), null, 0, 0, 0, null, null, false);
        }
        let startTime = Date.now();
        let timeout = (_a = opts.timeout) !== null && _a !== void 0 ? _a : -1;
        let timeStep = (_b = opts.timeStep) !== null && _b !== void 0 ? _b : 10; // ms
        let onProgress = (_c = opts.onProgress) !== null && _c !== void 0 ? _c : null;
        let onCleanup = (_d = opts.cleanup) !== null && _d !== void 0 ? _d : null;
        let usePostMessage = (_e = opts.usePostMessage) !== null && _e !== void 0 ? _e : true;
        return new BolusPromise(null, bolus, startTime, timeout, timeStep, onProgress, onCleanup, usePostMessage);
    }

    exports.BigFloat = BigFloat;
    exports.CompilationError = CompilationError;
    exports.Complex = Complex;
    exports.ParserError = ParserError;
    exports.ROUNDING_MODE = ROUNDING_MODE;
    exports.addMantissas = addMantissas;
    exports.asyncDigest = asyncDigest;
    exports.compileNode = compileNode;
    exports.complexToRGB = complexToRGB;
    exports.countFloatsBetween = countFloatsBetween;
    exports.floatStore = floatStore;
    exports.flrLog2 = flrLog2;
    exports.frExp = frExp;
    exports.gammaReal = gammaReal;
    exports.genVariableName = genVariableName;
    exports.getExponent = getExponent;
    exports.getExponentAndMantissa = getExponentAndMantissa;
    exports.getFloatStoreExponent = getFloatStoreExponent;
    exports.getFloatStoreMantissa = getFloatStoreMantissa;
    exports.getMantissa = getMantissa;
    exports.getTokens = getTokens;
    exports.getTrailingInfo = getTrailingInfo;
    exports.getWorkingPrecision = getWorkingPrecision;
    exports.getWorkingRM = getWorkingRM;
    exports.intView = intView;
    exports.integerExp = integerExp;
    exports.isDenormal = isDenormal;
    exports.isValidVariableName = isValidVariableName;
    exports.lnGammaReal = lnGammaReal;
    exports.mantissaClz = mantissaClz;
    exports.mantissaCtz = mantissaCtz;
    exports.neededWordsForPrecision = neededWordsForPrecision;
    exports.parseString = parseString;
    exports.pow2 = pow2;
    exports.prettyPrintMantissa = prettyPrintMantissa;
    exports.rationalExp = rationalExp;
    exports.resolveOperatorDefinition = resolveOperatorDefinition;
    exports.roundDown = roundDown;
    exports.roundMantissaToPrecision = roundMantissaToPrecision;
    exports.roundUp = roundUp;
    exports.setFloatStore = setFloatStore;
    exports.setWorkingPrecision = setWorkingPrecision;
    exports.setWorkingRM = setWorkingRM;
    exports.subtractMantissas = subtractMantissas;
    exports.syncDigest = syncDigest;
    exports.toBinary = toBinary;
    exports.toHex = toHex;
    exports.ulp = ulp;
    exports.ulpError = ulpError;
    exports.validateBigFloat = validateBigFloat;
    exports.validateMantissa = validateMantissa;
    exports.writeComplexToRGBA = writeComplexToRGBA;

    Object.defineProperty(exports, '__esModule', { value: true });

})));
