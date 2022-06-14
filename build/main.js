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
     * @param f Any floating-point number
     * @returns The next representable floating-point number, handling special cases
     */
    function roundUp(f) {
        if (f >= -POSITIVE_NORMAL_MIN && f < POSITIVE_NORMAL_MIN) {
            // denormal numbers
            return f + POSITIVE_DENORMAL_MIN;
        }
        else if (f === -Infinity) {
            // special case
            return -MAX_VALUE;
        }
        return f + Math.abs(f) * MAGIC_ROUND_C;
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
    let version = 0;
    /**
     * This function returns a number starting from 1 that never decreases. It is used to store "when" an operation has
     * occurred, and thus whether to consider it a change.
     * @returns {number}
     */
    function getVersionID() {
        return ++version;
    }
    // Generate an id of the form xxxx-xxxx
    // TODO: guarantee no collisions via LFSR or something similar
    function getStringID() {
        function randLetter() {
            return String.fromCharCode(Math.round(Math.random() * 25 + 97));
        }
        function randFourLetter() {
            return randLetter() + randLetter() + randLetter() + randLetter();
        }
        return randFourLetter() + '-' + randFourLetter();
    }
    /**
     * Simple deep equals. Uses Object.is-type equality, though. Doesn't handle circularity or any of the fancy new containers
     * @param x {*}
     * @param y {*}
     * @param lookForEqualsMethod {boolean} Whether to look for a method "equals()" on x to use instead of the standard method of comparison
     * @returns {boolean}
     */
    function deepEquals(x, y, lookForEqualsMethod = false) {
        if (typeof x !== 'object' || x === null)
            return Object.is(x, y);
        if (lookForEqualsMethod && x.equals)
            return x.equals(y);
        if (x.constructor !== y.constructor)
            return false;
        if (Array.isArray(x) && Array.isArray(y)) {
            if (x.length !== y.length)
                return false;
            for (let i = x.length - 1; i >= 0; --i) {
                if (!deepEquals(x[i], y[i], lookForEqualsMethod))
                    return false;
            }
            return true;
        }
        // The only other thing of consequence to us. Could probably handle other weird objects too, but meh.
        if (isTypedArray(x) && isTypedArray(y)) {
            if (x.length !== y.length)
                return false;
            if (x instanceof Float32Array || x instanceof Float64Array) {
                for (let i = x.length - 1; i >= 0; --i) {
                    const xv = x[i];
                    // What a beautiful way to test for same valueness between floats!
                    if ((xv !== y[i] && !(xv !== xv && y[i] !== y[i])) ||
                        (xv === 0 && 1 / xv !== 1 / y[i]))
                        return false;
                }
            }
            else {
                for (let i = x.length - 1; i >= 0; --i) {
                    if (x[i] !== y[i])
                        return false;
                }
            }
            return true;
        }
        if (x instanceof Map || x instanceof Set)
            return false; // Just in case
        // x and y are just objects
        const keys = Object.keys(x);
        if (Object.keys(y).length !== keys.length)
            return false;
        for (const key of keys) {
            // fails if y is Object.create(null)
            if (!y.hasOwnProperty(key))
                return false;
            if (!deepEquals(x[key], y[key]))
                return false;
        }
        return true;
    }
    function isTypedArray(arr) {
        return ArrayBuffer.isView(arr) && !(arr instanceof DataView);
    }
    /**
     * Arithmetic mod function instead of remainder
     * @param n {number}
     * @param m {number}
     * @returns {number}
     */
    function mod(n, m) {
        let r = n % m;
        return (r >= 0) ? r : (r + m);
    }
    // Remember left-pad?
    function leftZeroPad(str, len, char = '0') {
        var _a;
        if (str.length >= len)
            return str;
        char = (_a = char[0]) !== null && _a !== void 0 ? _a : '0';
        return char.repeat(len - str.length) + str;
    }
    const warnings = new Map();
    /**
     * Convenience function allowing local warnings that call console.warn, but do not repeatedly warn if the warning is
     * reached multiple times (incl. in different execution paths), to avoid spamming the console. The warnings are recorded
     * according to their id, which should be a string that compactly and uniquely describes the warning.
     * @param s Verbose warning description
     * @param id Short identifier for the warning (must be unique, unless multiple warnings should be shared)
     * @param maxCount The maximum number of warnings that should be sent to console.warn before output is silenced
     */
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
     * A real interval with only min, max, defMin (bit 0), defMax (bit 1), contMin (bit 2), contMax (bit 3)
     * TODO: types, functions
     */
    class RealInterval {
        constructor(min = 0, max = min, info = 0b1111) {
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
        contMin() {
            return this.info & 0b100;
        }
        contMax() {
            return this.info & 0b1000;
        }
        static fromObj(o, correctRounding = false) {
            let min = 0, max = 0, info = 0b1111, isStr = false;
            if (typeof o === "string") {
                isStr = true;
                if (correctRounding && isStringInteger(o)) {
                    correctRounding = false;
                }
                o = +o;
            }
            if (typeof o === "number") {
                if (Number.isNaN(o)) {
                    info = 0;
                }
                else {
                    min = max = o;
                    if (isStr && correctRounding) { // safely include the number
                        min = roundDown(min);
                        max = roundUp(max);
                    }
                }
            }
            else if (typeof o === "object" && o !== null) {
                // @ts-ignore
                if (o.min !== undefined && o.max !== undefined)
                    min = +o.min, max = +o.max;
                // @ts-ignore
                if (o.info !== undefined)
                    info = +o.info;
            }
            return new RealInterval(min, max, info);
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
                dst.info = 0b1111;
            }
        }
        static setRange(min, max, dst) {
            dst.min = min;
            dst.max = max;
            dst.info = 0b1111;
        }
        /**
         * Add two fast real intervals, sending the result to dst
         * @param src1 {RealInterval}
         * @param src2 {RealInterval}
         * @param dst {RealInterval}
         * @param correctRounding {boolean} Whether to use correct rounding so the result is mathematically guaranteed
         */
        static add(src1, src2, dst, correctRounding = false) {
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
         * @param src1 {RealInterval}
         * @param src2 {RealInterval}
         * @param dst {RealInterval}
         * @param correctRounding {boolean} Whether to use correct rounding so the result is mathematically guaranteed
         */
        static sub(src1, src2, dst, correctRounding = false) {
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
         * @param src {RealInterval}
         * @param dst {RealInterval}
         * @param correctRounding {boolean} Whether to use correct rounding so the result is mathematically guaranteed
         */
        static unaryMinus(src, dst, correctRounding = false) {
            dst.min = -src.max;
            dst.max = -src.min;
            dst.info = src.info;
        }
        /**
         * Multiply two fast real intervals, sending the result to dst
         * @param src1 {RealInterval}
         * @param src2 {RealInterval}
         * @param dst {RealInterval}
         * @param correctRounding {boolean} Whether to use correct rounding so the result is mathematically guaranteed
         */
        static mul(src1, src2, dst, correctRounding = false) {
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
         * @param src1 {RealInterval}
         * @param src2 {RealInterval}
         * @param dst {RealInterval}
         * @param correctRounding {boolean} Whether to use correct rounding so the result is mathematically guaranteed
         */
        static div(src1, src2, dst, correctRounding = false) {
            let info = src1.info & src2.info;
            if (info === 0) {
                dst.info = 0;
                return;
            }
            let s2min = src2.min, s2max = src2.max;
            if (0 < s2min || 0 > s2max) {
                // if 0 is outside the range then there are no singularities
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
                dst.info = info & 0b0101;
            }
        }
        /**
         * Take the square root of a real interval, sending the result to dst
         * @param src {RealInterval}
         * @param dst {RealInterval}
         * @param correctRounding {boolean} Whether to use correct rounding so the result is mathematically guaranteed
         */
        static sqrt(src, dst, correctRounding = false) {
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
         * @param src {RealInterval}
         * @param dst {RealInterval}
         * @param correctRounding {boolean} Whether to use correct rounding so the result is mathematically guaranteed
         */
        static cbrt(src, dst, correctRounding = false) {
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
         * @param src {RealInterval}
         * @param dst {RealInterval}
         * @param correctRounding {boolean} Whether to use correct rounding so the result is mathematically guaranteed TODO
         */
        static sin(src, dst, correctRounding = false) {
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
         * @param src {RealInterval}
         * @param dst {RealInterval}
         * @param correctRounding {boolean} Whether to use correct rounding so the result is mathematically guaranteed TODO
         */
        static cos(src, dst, correctRounding = false) {
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
         * @param src {RealInterval}
         * @param dst {RealInterval}
         * @param correctRounding {boolean} Whether to use correct rounding so the result is mathematically guaranteed TODO
         */
        static tan(src, dst, correctRounding = false) {
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
         * @param src {RealInterval}
         * @param dst {RealInterval}
         * @param correctRounding {boolean} Whether to use correct rounding so the result is mathematically guaranteed
         */
        static asin(src, dst, correctRounding = false) {
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
                    info &= 0b1010;
                }
                else {
                    min = Math.asin(srcMin);
                }
                if (srcMax > 1) {
                    max = Math.PI / 2;
                    info &= 0b1010;
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
         * @param src {RealInterval}
         * @param dst {RealInterval}
         * @param correctRounding {boolean} Whether to use correct rounding so the result is mathematically guaranteed
         */
        static acos(src, dst, correctRounding = false) {
            let info = src.info;
            if (info === 0) {
                dst.info = 0;
                return;
            }
            let srcMin = src.min, srcMax = src.max;
            if (srcMax < -1 || srcMin > 1) {
                dst.info = 0; // complete domain error
                return;
            }
            let min, max;
            if (srcMin === srcMax) {
                min = max = Math.acos(srcMin);
            }
            else {
                if (srcMin < -1) {
                    max = Math.PI;
                    // domain error
                    info &= 0b1010;
                }
                else {
                    max = Math.acos(srcMin);
                }
                if (srcMax > 1) {
                    min = 0;
                    info &= 0b1010;
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
         * @param src {RealInterval}
         * @param dst {RealInterval}
         * @param correctRounding {boolean} Whether to use correct rounding so the result is mathematically guaranteed
         */
        static atan(src, dst, correctRounding = false) {
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
         * @param src1 {RealInterval}
         * @param src2 {RealInterval}
         * @param dst {RealInterval}
         * @param correctRounding {boolean}
         */
        static pow(src1, src2, dst, correctRounding = false) {
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
                                dst.info = info & 0b1010;
                            }
                            else {
                                // Odd integers: if contains zero, contains an asymptote
                                // 1/x
                                dst.min = Infinity;
                                dst.max = Infinity;
                                dst.info = info & 0b1010;
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
                        dst.info = info & 0b1010;
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
                    dst.info = info & (containsZero ? 0b1011 : 0b1111);
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
                    dst.info = info & 0b1010;
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
                    info &= 0b1010;
                }
                else if (base < 0) {
                    // Shape: (-1/2)^x
                    let m = Math.pow(base, s2min);
                    min = -m;
                    max = m;
                    info &= 0b1010;
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
                dst.info = info & 0b1010;
                return;
            }
            // Things are potentially undefined iff the denominator has negative numbers.
            let isAllDefined = s1min < 0;
            if (isAllDefined)
                info &= 0b1010;
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
        /**
         * Get the natural logarithm of a real interval
         * @param src
         * @param dst
         * @param correctRounding
         */
        static ln(src, dst, correctRounding = false) {
            let info = src.info;
            if (info === 0) {
                dst.info = 0;
                return;
            }
            let srcMin = src.min;
            let srcMax = src.max;
            let lnMin = Math.log(srcMin);
            let lnMax = Math.log(srcMax);
            // monotonically increasing, so our job is pretty easy
            if (srcMin <= 0) {
                info &= 0b1010; // def min, cont min
            }
            else if (srcMax <= 0) {
                info = 0; // entirely undefined
            }
            dst.min = lnMin;
            dst.max = lnMax;
            dst.info = info;
        }
        /**
         * Get the logarithm base 10 of a real interval
         * @param src
         * @param dst
         * @param correctRounding
         */
        static log10(src, dst, correctRounding = false) {
            let info = src.info;
            if (info === 0) {
                dst.info = 0;
                return;
            }
            let srcMin = src.min;
            let srcMax = src.max;
            let lnMin = Math.log10(srcMin);
            let lnMax = Math.log10(srcMax);
            // monotonically increasing, so our job is pretty easy
            if (srcMin <= 0) {
                info &= 0b1010; // def min, cont min
            }
            else if (srcMax <= 0) {
                info = 0; // entirely undefined
            }
            dst.min = lnMin;
            dst.max = lnMax;
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

    // Series acceleration terms (see Python code)
    const coeffs = [
        30122754096401.0,
        30122754096400.0,
        -30122754095752.0,
        30122754025984.0,
        -30122751049216.0,
        30122684071936.0,
        -30121767227392.0,
        30113460060160.0,
        -30060878430208.0,
        29819879292928.0,
        -29000797257728.0,
        26905671630848.0,
        -22842397687808.0,
        16865262829568.0,
        -10244436525056.0,
        4814658338816.0,
        -1619202670592.0,
        343597383680.0,
        -34359738368.0
    ];
    const n = 18;
    // [...new Array(18).keys()].map(i => i+1).map(Math.log)
    let precomputedLogs = [
        NaN,
        0,
        0.6931471805599453,
        1.0986122886681096,
        1.3862943611198906,
        1.6094379124341003,
        1.791759469228055,
        1.9459101490553132,
        2.0794415416798357,
        2.1972245773362196,
        2.302585092994046,
        2.3978952727983707,
        2.4849066497880004,
        2.5649493574615367,
        2.6390573296152584,
        2.70805020110221,
        2.772588722239781,
        2.833213344056216,
        2.8903717578961645
    ];
    function riemannZetaReal(x) {
        let s = 0;
        if (x > 12) {
            // extremely close to 1, use the canonical series definition
            for (let k = 1; k <= n; ++k) {
                s += Math.exp(precomputedLogs[k] * -x);
            }
            return s;
        }
        for (let k = 1; k <= n; ++k) {
            let term = coeffs[k] * Math.exp(precomputedLogs[k] * -x);
            s += term;
        }
        return s / (coeffs[0] * (1 - Math.exp(precomputedLogs[2] * (1 - x))));
    }

    /**
     * Normal-precision complex number.
     */
    class Complex {
        constructor(re = 0, im = 0) {
            this.re = +re;
            this.im = +im;
        }
        setComponents(re, im = 0) {
            this.re = re;
            this.im = im;
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
         * See Complex.approxArg
         * @param re
         * @param im
         */
        static approxArgComponents(re, im) {
            let arg = 0;
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
            return arg;
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
        expi(theta) {
            this.re = Math.cos(theta);
            this.im = Math.sin(theta);
        }
        /**
         * Set this complex number to the result of b^r
         * @param b {Complex}
         * @param exp {number}
         */
        powReal(b, exp) {
            if (Number.isInteger(exp) && exp >= 0) {
                if (exp === 0) { // TODO add more integer handling
                    this.re = 1;
                    this.im = 0;
                    return;
                }
                let r = b.re, i = b.im;
                if (exp === 1) {
                    this.re = b.re;
                    this.im = b.im;
                    return;
                }
                else if (exp === 2) {
                    // square b
                    this.re = r * r - i * i;
                    this.im = 2 * r * i;
                    return;
                }
                else if (exp === 3) {
                    this.re = r * r * r - 3 * r * i * i;
                    this.im = 3 * r * r * i - i * i * i;
                    return;
                }
                else if (exp === 4) {
                    let i2 = i * i;
                    let r2 = r * r;
                    this.re = r2 * r2 - 6 * r2 * i2 + i2 * i2;
                    this.im = 4 * (r2 * r * i - i * i2 * r);
                    return;
                }
                else if (exp === 5) {
                    let i2 = i * i;
                    let r2 = r * r;
                    let r3 = r2 * r;
                    this.re = r2 * r3 - 10 * r3 * i2 + 5 * r * i2 * i2;
                    this.im = 5 * r2 * r2 * i - 10 * r2 * i2 * i + i2 * i2 * i;
                    return;
                }
            }
            let c = new Complex();
            c.log(b);
            c.multiplyReal(c, exp);
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
            return Complex.approxArgComponents(c.re, c.im);
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
        init: () => new RealInterval(0, 0, 0b1111),
        typecheck: b => b instanceof RealInterval,
        clone: b => new RealInterval(b.min, b.max, b.info),
        copyTo: (src, dst) => { dst.min = src.min; dst.max = src.max; dst.info = src.info; },
        castPermissive: RealInterval.fromObj
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
            "int": "interval_int",
            "real": "interval_real",
            "bool": "interval_bool"
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
            }),
            new ConcreteEvaluator({
                args: ["interval_real", "interval_real"],
                returns: "interval_real",
                evalType: "write",
                func: RealInterval.add
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
            }),
            new ConcreteEvaluator({
                args: ["interval_real", "interval_real"],
                returns: "interval_real",
                evalType: "write",
                func: RealInterval.mul
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
            }),
            new ConcreteEvaluator({
                args: ["interval_real", "interval_real"],
                returns: "interval_real",
                evalType: "write",
                func: RealInterval.div
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
            }),
            new ConcreteEvaluator({
                args: ["interval_real", "interval_real"],
                returns: "interval_real",
                evalType: "write",
                func: RealInterval.sub
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
            }),
            new ConcreteEvaluator({
                args: ["interval_real", "interval_real"],
                returns: "interval_real",
                evalType: "write",
                func: RealInterval.unaryMinus
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
            }),
            new ConcreteEvaluator({
                args: ["interval_real", "interval_real"],
                returns: "interval_real",
                evalType: "write",
                func: RealInterval.pow
            })
        ]
    }));
    // TODO: alias
    registerOperator(new OperatorDefinition({
        name: 'pow',
        args: ["real", "real"],
        returns: "real",
        evaluators: [
            new ConcreteEvaluator({
                args: ["real", "real"],
                returns: "real",
                func: Math.pow
            }),
            new ConcreteEvaluator({
                args: ["interval_real", "interval_real"],
                returns: "interval_real",
                evalType: "write",
                func: RealInterval.pow
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
            }),
            new ConcreteEvaluator({
                args: ["interval_real", "interval_real"],
                returns: "interval_real",
                evalType: "write",
                func: RealInterval.ln
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
         * Figure out the type of each node, given the type of each variable node within it. Examples:
         *
         * node = Grapheme.parseString("(x+y)^2+cos(z)").resolveTypes({ z: "real" }, { defaultType: "complex" })
         *    -> Node which takes in a real value z and two complex values x and y
         * node = Grapheme.parseString("(x+y)^2+cos(z)").resolveTypes({}, { defaultType: "complex" })
         *    -> Node which takes in three complex values
         * node = Grapheme.parseString("(x+y)^2+cos(z)").resolveTypes()
         *    -> Node which takes in three real values
         * node = Grapheme.parseString("cowbell(z)")    // (does not throw)
         * node.resolveTypes()                          // throws
         *
         * Perf: on "x^2+y^2+e^-x^2+pow(3,gamma(2401 + complex(2,3)))", took 0.002 ms / iteration as of Mar 14, 2022
         *
         * @param vars {{}} Mapping from variable names to their types
         * @param opts
         */
        resolveTypes(vars, opts = {}) {
            // Convert all arg values to mathematical types
            let { defaultType = "real", throwOnUnresolved = true } = opts;
            vars !== null && vars !== void 0 ? vars : (vars = {});
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
                revisedVars[v] = toMathematicalType(n, true /* throw on error */);
            }
            let revisedType = toMathematicalType(defaultType, true);
            this.applyAll(node => node._resolveTypes({
                vars: revisedVars, throwOnUnresolved, defaultType: revisedType
            }), false /* only groups */, true /* children first */);
            return this;
        }
        /**
         * Whether all operator definitions and types have been resolved for this expression by checking whether the type of
         * this node is known. If it is not known, then at least one of the types of its children is not known either
         */
        allResolved() {
            return !!(this.type);
        }
        /**
         * Internal resolve types function recursively called
         */
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
                throw new EvaluationError(`[E0001] This node has not had its types fully resolved (call .resolveTypes()).\
In other words, the node cannot be used for computation until an abstract type and operator definition has been found for each node.`);
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
                            throw new EvaluationError(`[E0001] Type of variable ${name} has not been resolved`);
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
                throw new EvaluationError(`[E0001] Type of constant variable with value ${this.value} has not been resolved`);
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
            var _a, _b;
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
                    let msg = (_b = (_a = concreteType.typecheckVerbose) === null || _a === void 0 ? void 0 : _a.call(concreteType, v)) !== null && _b !== void 0 ? _b : "";
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
            let childrenValues = [];
            let children = this.children;
            let cl = children.length;
            for (let i = 0; i < cl; ++i) {
                let c = children[i];
                let cast = casts[i];
                let ccast = cast.getDefaultEvaluator(mode);
                if (ccast === null) {
                    throw new EvaluationError(`No concrete cast (in mode ${mode.name}) from source ${mode.getConcreteType(cast.srcType()).toHashStr()}`
                        + ` to destination ${mode.getConcreteType(cast.dstType()).toHashStr()}`);
                }
                let uncastedChild = c._evaluate(vars, mode, opts);
                childrenValues.push(ccast.callNew([
                    uncastedChild
                ]));
            }
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
            this.inputFormat = inputFormat;
            // Scope typecheck
            if (usesScope && typechecks) {
                let f = new TypecheckScopeFragment();
                f.scopeName = "scope";
                this.add(f);
            }
            /**
             * Step 1: extract variables from scope or from arguments
             */
            for (let [name, node] of neededInputs) {
                if (inputFormat.includes(name)) ;
                else {
                    if (usesScope) {
                        let f = new VariableDefinitionCodeFragment();
                        f.name = name;
                        f.value = `scope.${name}`;
                        f.verbatim = true;
                        f.construct = true; // In this case, we do construct the variable directly since we're extracting it
                        this.add(f);
                    }
                    else {
                        throw new CompilationError(`Can't find variable ${name}`);
                    }
                }
                if (typechecks) {
                    let f = new TypecheckFragment();
                    f.name = name;
                    f.concreteType = node.type;
                    this.add(f);
                }
            }
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
                    this.add(`return ${cGraph.root};`);
                }
            }
            let inputCTypes = this.inputFormat.map(varName => {
                if (varName === "scope") {
                    return "scope";
                }
                let n = cGraph.nodes.get(varName);
                if (!n) { // Unused input node
                    return "any";
                }
                return n.type;
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
            exportText += "};";
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
    /**
     * Fragment which throws an error if a typecheck fails. There is no "error code"–type handling for typecheck failures,
     * because they are so damaging to performance that it would make no sense to use them. Essentially, if the wrong type
     * is passed to a compiled function, it will have to be (partially or fully) deoptimized, defeating its purpose.
     */
    class TypecheckFragment {
        compileToEnv(env) {
            let t = this.concreteType;
            let tc = t.typecheck;
            let tcv = t.typecheckVerbose;
            if (!tc) {
                // TODO make warning?
                throw new CompilationError(`No defined typecheck for concrete type ${t.toHashStr()}`);
            }
            let typecheckFast = env.importFunction(tc);
            let typecheckVerbose = tcv ? env.importFunction(tcv) : ""; // prevent doodoo
            let name = this.name;
            let inner = (typecheckVerbose) ?
                `throw new ${env.importValue(EvaluationError)}("Variable ${name} failed typecheck: " + ${typecheckVerbose}(${name}));`
                : `throw new ${env.importValue(EvaluationError)}("Variable ${name} failed typecheck");`; // verbose not available
            // First perform a fast typecheck, then, if it fails, throw a verbose message
            env.addMain(`if (!${typecheckFast}(${this.name})) {
      ${inner}
    }`);
        }
    }
    function checkScopeFast(scope) {
        return typeof scope === "object" && scope !== null;
    }
    function checkScopeVerbose(scope) {
        if (!checkScopeFast(scope))
            return "Expected scope object (i.e., associative array object with variables as keys and values as the variables' values)";
        return "";
    }
    /**
     * Typecheck the scope object in particular
     */
    class TypecheckScopeFragment {
        compileToEnv(env) {
            let scopeTypecheckFast = env.importFunction(checkScopeFast);
            let scopeTypecheckVerbose = env.importFunction(checkScopeVerbose);
            let scopeName = this.scopeName;
            env.addMain(`if (!${scopeTypecheckFast}(${scopeName})) {
    throw new ${env.importValue(EvaluationError)}("Variable ${scopeName} failed typecheck: " + ${scopeTypecheckVerbose}(${scopeName}));
    }`);
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
                    stringValue: mNode.value,
                    value: cType.castPermissive(mNode.value)
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
        // Resolve nodes if necessary
        if (!root.allResolved()) {
            root.resolveTypes((_a = options.variables) !== null && _a !== void 0 ? _a : {}, Object.assign(Object.assign({}, options.resolveTypes), { throwOnUnresolved: true }));
        }
        // Uniformize properties
        let targetOpts = options.targets;
        if (!targetOpts)
            targetOpts = {};
        let dt = Object.assign({}, defaultTarget);
        if (options.inputFormat)
            dt.inputFormat = options.inputFormat;
        if (options.mode)
            dt.mode = options.mode;
        if (!Array.isArray(targetOpts))
            targetOpts = [targetOpts]; // default is a single target (targets[0])
        let rootProperties = {
            usedVariables: root.getVariableDependencies()
        };
        targetOpts = targetOpts;
        // Convert each target to a full target
        let targets = [];
        for (let i = 0; i < targetOpts.length; ++i) {
            // Merge default options
            let to = targetOpts[i];
            to = Object.assign(Object.assign({}, dt), to);
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

    function _clampRGB(x) {
        return (((x < 0) ? 0 : ((x > 255) ? 255 : x)) + 0.5) | 0;
    }
    function throwBadColor(s) {
        throw new Error('Unrecognized colour ' + s);
    }
    class Color {
        constructor(r, g, b, a) {
            this.r = r;
            this.g = g;
            this.b = b;
            this.a = a;
        }
        /**
         * Convert RGB to color, clamping the values to the appropriate range. Each component should be in the range [0,255].
         * @param r Red component
         * @param g Green component
         * @param b Blue component
         */
        static rgb(r, g, b) {
            return new Color(_clampRGB(r), _clampRGB(g), _clampRGB(b), 255);
        }
        /**
         * Convert RGBA to color, clamping the values to the appropriate range. Each component should be in the range [0,255].
         * @param r Red component
         * @param g Green component
         * @param b Blue component
         * @param a Opacity component
         */
        static rgba(r, g, b, a = 255) {
            return new Color(_clampRGB(r), _clampRGB(g), _clampRGB(b), _clampRGB(a));
        }
        /**
         * Convert HSL to Color. The hue should be in radians, the saturation in [0,1], and the luminance in [0,1].
         * @param h Hue
         * @param s Saturation
         * @param l Luminance
         */
        static hsl(h, s, l) {
            let [r, g, b] = hslToRgb(h * (1 / (2 * Math.PI)), s * (1 / 100), l * (1 / 100));
            return Color.rgb(r, g, b);
        }
        /**
         * Convert HSLA to Color. The hue should be in radians, the saturation in [0,1], the luminance in [0,1], and the
         * opacity in [0,255].
         * @param h Hue
         * @param s Saturation
         * @param l Luminance
         * @param a Opacity
         */
        static hsla(h, s, l, a) {
            let color = Color.hsl(h, s, l);
            color.a = _clampRGB(a);
            return color;
        }
        /**
         * Convert from hex string ("#231", "#00ff22" or "#00ff22aa") to Color.
         * @param s Hex string
         */
        static fromHex(s) {
            return hexToRgb(s);
        }
        /**
         * Convert from a (subset) of CSS color strings to a Color. Supported formats are hsla(degrees, percent, percent,
         * [0,1]), hsl(...), rgb(...), rgba(...), named colors like "blue", and hex strings. An error is thrown on an unknown
         * color.
         * @param s CSS string
         */
        static fromCss(s) {
            s = s.toLowerCase().replace(/\s+/g, '');
            if (s.startsWith('#')) {
                return Color.fromHex(s);
            }
            let argsMatch = /\((.+)\)/g.exec(s);
            if (!argsMatch) {
                let color = Colors[s.toUpperCase()];
                return color ? color : throwBadColor(s);
            }
            let args = argsMatch[1].split(',').map(Number.parseFloat);
            if (s.startsWith('rgb')) {
                return Color.rgba(args[0], args[1], args[2], s.startsWith('rgba') ? (_clampRGB(args[3] * 255)) : 255);
            }
            else if (s.startsWith('hsl')) {
                // args[1] and args[2] are percentages and so must be scaled. args[0], the luminance, is in [0, 255]
                args[0] *= Math.PI / 180;
                if (s[3] === 'a') {
                    return Color.hsla(args[0], args[1], args[2], args[3] * 255);
                }
                return Color.hsl(args[0], args[1], args[2]);
            }
            throwBadColor(s);
        }
        static compose(...args) {
            if (args.length === 0) {
                return Color.default();
            }
            // Use the last specification
            return Color.fromObj(args[args.length - 1]);
        }
        static create(spec) {
            return Color.fromObj(spec);
        }
        /**
         * Permissively convert an object to a color; returns black if conversion failed
         */
        static fromObj(obj) {
            var _a;
            if (typeof obj === 'string') {
                return Color.fromCss(obj);
            }
            // @ts-ignore
            if (obj && typeof obj.r === 'number' && typeof obj.g === 'number' && typeof obj.b === 'number') {
                // @ts-ignore
                return Color.rgba(obj.r, obj.g, obj.b, (_a = obj.a) !== null && _a !== void 0 ? _a : 255);
            }
            return Color.default();
        }
        static default() {
            return Color.rgba(0, 0, 0, 255);
        }
        rounded() {
            return {
                r: (this.r + 0.5) | 0,
                g: (this.g + 0.5) | 0,
                b: (this.b + 0.5) | 0,
                a: (this.a + 0.5) | 0
            };
        }
        /**
         * Convert color to a CSS-style hex string
         */
        hex() {
            const rnd = this.rounded();
            return `#${[rnd.r, rnd.g, rnd.b, rnd.a]
            .map(x => leftZeroPad(x.toString(16), 2))
            .join('')}`;
        }
        /**
         * Convert this color into a packed hex number (including an opacity value)
         */
        toNumber() {
            return this.r * 0x1000000 + this.g * 0x10000 + this.b * 0x100 + this.a;
        }
        clone() {
            return new Color(this.r, this.g, this.b, this.a);
        }
        equals(c) {
            return c.r === this.r && c.g === this.g && c.b === this.b && c.a === this.a;
        }
    }
    function hexToRgb(hex) {
        hex = hex.replace(/#/g, '').trim();
        let r = 0, g = 0, b = 0, a = 255;
        if (hex.length === 3 || hex.length === 4) {
            r = Number.parseInt(hex[0], 16);
            g = Number.parseInt(hex[1], 16);
            b = Number.parseInt(hex[2], 16);
            if (hex.length === 4) {
                a = Number.parseInt(hex[3], 16);
            }
        }
        else if (hex.length === 6 || hex.length === 8) {
            r = Number.parseInt(hex.slice(0, 2), 16);
            g = Number.parseInt(hex.slice(2, 4), 16);
            b = Number.parseInt(hex.slice(4, 6), 16);
            if (hex.length === 8) {
                a = Number.parseInt(hex.slice(6, 8), 16);
            }
        }
        else {
            throwBadColor(hex);
        }
        return Color.rgba(r, g, b, a);
    }
    // Credit to https://stackoverflow.com/a/9493060/13458117
    function hue2Rgb(p, q, t) {
        if (t < 0)
            t += 1;
        if (t > 1)
            t -= 1;
        if (t < 1 / 6)
            return p + (q - p) * 6 * t;
        if (t < 1 / 2)
            return q;
        if (t < 2 / 3)
            return p + (q - p) * (2 / 3 - t) * 6;
        return p;
    }
    function hslToRgb(h, s, l) {
        // h, s, l in [0,1]
        let r, g, b;
        if (s === 0) {
            r = g = b = l; // achromatic
        }
        else {
            var q = l < 0.5 ? l * (1 + s) : l + s - l * s;
            var p = 2 * l - q;
            r = hue2Rgb(p, q, h + 1 / 3);
            g = hue2Rgb(p, q, h);
            b = hue2Rgb(p, q, h - 1 / 3);
        }
        return [255 * r, 255 * g, 255 * b];
    }
    const rgb = Color.rgb;
    const Colors = Object.freeze({
        get LIGHTSALMON() {
            return rgb(255, 160, 122);
        },
        get SALMON() {
            return rgb(250, 128, 114);
        },
        get DARKSALMON() {
            return rgb(233, 150, 122);
        },
        get LIGHTCORAL() {
            return rgb(240, 128, 128);
        },
        get INDIANRED() {
            return rgb(205, 92, 92);
        },
        get CRIMSON() {
            return rgb(220, 20, 60);
        },
        get FIREBRICK() {
            return rgb(178, 34, 34);
        },
        get RED() {
            return rgb(255, 0, 0);
        },
        get DARKRED() {
            return rgb(139, 0, 0);
        },
        get CORAL() {
            return rgb(255, 127, 80);
        },
        get TOMATO() {
            return rgb(255, 99, 71);
        },
        get ORANGERED() {
            return rgb(255, 69, 0);
        },
        get GOLD() {
            return rgb(255, 215, 0);
        },
        get ORANGE() {
            return rgb(255, 165, 0);
        },
        get DARKORANGE() {
            return rgb(255, 140, 0);
        },
        get LIGHTYELLOW() {
            return rgb(255, 255, 224);
        },
        get LEMONCHIFFON() {
            return rgb(255, 250, 205);
        },
        get LIGHTGOLDENRODYELLOW() {
            return rgb(250, 250, 210);
        },
        get PAPAYAWHIP() {
            return rgb(255, 239, 213);
        },
        get MOCCASIN() {
            return rgb(255, 228, 181);
        },
        get PEACHPUFF() {
            return rgb(255, 218, 185);
        },
        get PALEGOLDENROD() {
            return rgb(238, 232, 170);
        },
        get KHAKI() {
            return rgb(240, 230, 140);
        },
        get DARKKHAKI() {
            return rgb(189, 183, 107);
        },
        get YELLOW() {
            return rgb(255, 255, 0);
        },
        get LAWNGREEN() {
            return rgb(124, 252, 0);
        },
        get CHARTREUSE() {
            return rgb(127, 255, 0);
        },
        get LIMEGREEN() {
            return rgb(50, 205, 50);
        },
        get LIME() {
            return rgb(0, 255, 0);
        },
        get FORESTGREEN() {
            return rgb(34, 139, 34);
        },
        get GREEN() {
            return rgb(0, 128, 0);
        },
        get DARKGREEN() {
            return rgb(0, 100, 0);
        },
        get GREENYELLOW() {
            return rgb(173, 255, 47);
        },
        get YELLOWGREEN() {
            return rgb(154, 205, 50);
        },
        get SPRINGGREEN() {
            return rgb(0, 255, 127);
        },
        get MEDIUMSPRINGGREEN() {
            return rgb(0, 250, 154);
        },
        get LIGHTGREEN() {
            return rgb(144, 238, 144);
        },
        get PALEGREEN() {
            return rgb(152, 251, 152);
        },
        get DARKSEAGREEN() {
            return rgb(143, 188, 143);
        },
        get MEDIUMSEAGREEN() {
            return rgb(60, 179, 113);
        },
        get SEAGREEN() {
            return rgb(46, 139, 87);
        },
        get OLIVE() {
            return rgb(128, 128, 0);
        },
        get DARKOLIVEGREEN() {
            return rgb(85, 107, 47);
        },
        get OLIVEDRAB() {
            return rgb(107, 142, 35);
        },
        get LIGHTCYAN() {
            return rgb(224, 255, 255);
        },
        get CYAN() {
            return rgb(0, 255, 255);
        },
        get AQUA() {
            return rgb(0, 255, 255);
        },
        get AQUAMARINE() {
            return rgb(127, 255, 212);
        },
        get MEDIUMAQUAMARINE() {
            return rgb(102, 205, 170);
        },
        get PALETURQUOISE() {
            return rgb(175, 238, 238);
        },
        get TURQUOISE() {
            return rgb(64, 224, 208);
        },
        get MEDIUMTURQUOISE() {
            return rgb(72, 209, 204);
        },
        get DARKTURQUOISE() {
            return rgb(0, 206, 209);
        },
        get LIGHTSEAGREEN() {
            return rgb(32, 178, 170);
        },
        get CADETBLUE() {
            return rgb(95, 158, 160);
        },
        get DARKCYAN() {
            return rgb(0, 139, 139);
        },
        get TEAL() {
            return rgb(0, 128, 128);
        },
        get POWDERBLUE() {
            return rgb(176, 224, 230);
        },
        get LIGHTBLUE() {
            return rgb(173, 216, 230);
        },
        get LIGHTSKYBLUE() {
            return rgb(135, 206, 250);
        },
        get SKYBLUE() {
            return rgb(135, 206, 235);
        },
        get DEEPSKYBLUE() {
            return rgb(0, 191, 255);
        },
        get LIGHTSTEELBLUE() {
            return rgb(176, 196, 222);
        },
        get DODGERBLUE() {
            return rgb(30, 144, 255);
        },
        get CORNFLOWERBLUE() {
            return rgb(100, 149, 237);
        },
        get STEELBLUE() {
            return rgb(70, 130, 180);
        },
        get ROYALBLUE() {
            return rgb(65, 105, 225);
        },
        get BLUE() {
            return rgb(0, 0, 255);
        },
        get MEDIUMBLUE() {
            return rgb(0, 0, 205);
        },
        get DARKBLUE() {
            return rgb(0, 0, 139);
        },
        get NAVY() {
            return rgb(0, 0, 128);
        },
        get MIDNIGHTBLUE() {
            return rgb(25, 25, 112);
        },
        get MEDIUMSLATEBLUE() {
            return rgb(123, 104, 238);
        },
        get SLATEBLUE() {
            return rgb(106, 90, 205);
        },
        get DARKSLATEBLUE() {
            return rgb(72, 61, 139);
        },
        get LAVENDER() {
            return rgb(230, 230, 250);
        },
        get THISTLE() {
            return rgb(216, 191, 216);
        },
        get PLUM() {
            return rgb(221, 160, 221);
        },
        get VIOLET() {
            return rgb(238, 130, 238);
        },
        get ORCHID() {
            return rgb(218, 112, 214);
        },
        get FUCHSIA() {
            return rgb(255, 0, 255);
        },
        get MAGENTA() {
            return rgb(255, 0, 255);
        },
        get MEDIUMORCHID() {
            return rgb(186, 85, 211);
        },
        get MEDIUMPURPLE() {
            return rgb(147, 112, 219);
        },
        get BLUEVIOLET() {
            return rgb(138, 43, 226);
        },
        get DARKVIOLET() {
            return rgb(148, 0, 211);
        },
        get DARKORCHID() {
            return rgb(153, 50, 204);
        },
        get DARKMAGENTA() {
            return rgb(139, 0, 139);
        },
        get PURPLE() {
            return rgb(128, 0, 128);
        },
        get INDIGO() {
            return rgb(75, 0, 130);
        },
        get PINK() {
            return rgb(255, 192, 203);
        },
        get LIGHTPINK() {
            return rgb(255, 182, 193);
        },
        get HOTPINK() {
            return rgb(255, 105, 180);
        },
        get DEEPPINK() {
            return rgb(255, 20, 147);
        },
        get PALEVIOLETRED() {
            return rgb(219, 112, 147);
        },
        get MEDIUMVIOLETRED() {
            return rgb(199, 21, 133);
        },
        get WHITE() {
            return rgb(255, 255, 255);
        },
        get SNOW() {
            return rgb(255, 250, 250);
        },
        get HONEYDEW() {
            return rgb(240, 255, 240);
        },
        get MINTCREAM() {
            return rgb(245, 255, 250);
        },
        get AZURE() {
            return rgb(240, 255, 255);
        },
        get ALICEBLUE() {
            return rgb(240, 248, 255);
        },
        get GHOSTWHITE() {
            return rgb(248, 248, 255);
        },
        get WHITESMOKE() {
            return rgb(245, 245, 245);
        },
        get SEASHELL() {
            return rgb(255, 245, 238);
        },
        get BEIGE() {
            return rgb(245, 245, 220);
        },
        get OLDLACE() {
            return rgb(253, 245, 230);
        },
        get FLORALWHITE() {
            return rgb(255, 250, 240);
        },
        get IVORY() {
            return rgb(255, 255, 240);
        },
        get ANTIQUEWHITE() {
            return rgb(250, 235, 215);
        },
        get LINEN() {
            return rgb(250, 240, 230);
        },
        get LAVENDERBLUSH() {
            return rgb(255, 240, 245);
        },
        get MISTYROSE() {
            return rgb(255, 228, 225);
        },
        get GAINSBORO() {
            return rgb(220, 220, 220);
        },
        get LIGHTGRAY() {
            return rgb(211, 211, 211);
        },
        get SILVER() {
            return rgb(192, 192, 192);
        },
        get DARKGRAY() {
            return rgb(169, 169, 169);
        },
        get GRAY() {
            return rgb(128, 128, 128);
        },
        get DIMGRAY() {
            return rgb(105, 105, 105);
        },
        get LIGHTSLATEGRAY() {
            return rgb(119, 136, 153);
        },
        get SLATEGRAY() {
            return rgb(112, 128, 144);
        },
        get DARKSLATEGRAY() {
            return rgb(47, 79, 79);
        },
        get BLACK() {
            return rgb(0, 0, 0);
        },
        get CORNSILK() {
            return rgb(255, 248, 220);
        },
        get BLANCHEDALMOND() {
            return rgb(255, 235, 205);
        },
        get BISQUE() {
            return rgb(255, 228, 196);
        },
        get NAVAJOWHITE() {
            return rgb(255, 222, 173);
        },
        get WHEAT() {
            return rgb(245, 222, 179);
        },
        get BURLYWOOD() {
            return rgb(222, 184, 135);
        },
        get TAN() {
            return rgb(210, 180, 140);
        },
        get ROSYBROWN() {
            return rgb(188, 143, 143);
        },
        get SANDYBROWN() {
            return rgb(244, 164, 96);
        },
        get GOLDENROD() {
            return rgb(218, 165, 32);
        },
        get PERU() {
            return rgb(205, 133, 63);
        },
        get CHOCOLATE() {
            return rgb(210, 105, 30);
        },
        get SADDLEBROWN() {
            return rgb(139, 69, 19);
        },
        get SIENNA() {
            return rgb(160, 82, 45);
        },
        get BROWN() {
            return rgb(165, 42, 42);
        },
        get MAROON() {
            return rgb(128, 0, 0);
        },
        get TRANSPARENT() {
            return new Color(0, 0, 0, 0);
        }
    });

    function approxAtan(x) {
        if (x > 1)
            return (Math.PI / 2 - approxAtan(1 / x));
        return (Math.PI / 4) * x - x * (x - 1) * (0.2447 + 0.0663 * x);
    }
    const scratch = new Uint8ClampedArray(4);
    // Method of converting complex to RGBA
    class ColoringScheme {
        writeComplexToRGBA(c, arr, index) {
            this.writeComplexArrayToRGBA([c], arr, index);
        }
        /**
         * Convert a complex number via this scheme. Don't use this function directly, unless only invoking it occasionally;
         * writeComplexArrayToRGBA is usually preferable.
         * @param c
         */
        complexToColor(c) {
            this.writeComplexArrayToRGBA([c], scratch, 0);
            return new Color(scratch[0], scratch[1], scratch[2], scratch[3]);
        }
    }
    function writeHL(h, l, arr, index) {
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
    function isComplexArray(c) {
        return c[0] instanceof Complex;
    }
    // A repeating color scheme resets its brightness at various magnitudes. "exponential" will reset at every integer power
    // of the base. "even" will reset at every multiple of the base. A normal color scheme simply has a mapping from
    // [0, inf) -> [0, 1] for lightness, based on arctan. The input is scaled by base.
    class StandardColoringScheme extends ColoringScheme {
        constructor(opts = {}) {
            var _a, _b, _c, _d, _e;
            super();
            this.type = (_a = opts.type) !== null && _a !== void 0 ? _a : "normal";
            console.log(opts);
            let base = +((_b = opts.base) !== null && _b !== void 0 ? _b : 2);
            if (base !== base || base < 0) {
                throw new Error("Invalid color scheme repeating base");
            }
            this.base = base;
            this.transformation = (_c = opts.transformation) !== null && _c !== void 0 ? _c : "atan";
            this.minLightness = (_d = opts.minLightness) !== null && _d !== void 0 ? _d : 0.2;
            this.maxLightness = (_e = opts.maxLightness) !== null && _e !== void 0 ? _e : 0.6;
            if (this.minLightness < 0 || this.minLightness > 1) {
                throw new Error("Invalid color scheme minLightness");
            }
            if (this.maxLightness < 0 || this.maxLightness > 1) {
                throw new Error("Invalid color scheme maxLightness");
            }
        }
        writeComplexArrayToRGBA(complexArr, arr, index) {
            // Somewhat optimized
            if (complexArr.length === 0)
                return;
            // Destructuring remains slow in Chrome, unfortunately, so do this
            let type = this.type, base = this.base; this.transformation;
            let minLightness = this.minLightness;
            let maxLightness = this.maxLightness;
            let rLightness = maxLightness - minLightness;
            let isNormal = type === "normal";
            let isExponential = type === "repeat_exponential";
            let logBase = Math.log(this.base);
            console.log(base);
            function write(re, im, index) {
                let arg = Complex.approxArgComponents(re, im);
                let magnitude = Math.sqrt(re * re + im * im);
                let h = (arg + 2 * Math.PI / 3) * (1 / (2 * Math.PI));
                if (isNormal) {
                    writeHL(h, 2 / Math.PI * approxAtan(magnitude * base), arr, index);
                    return;
                }
                else if (isExponential) {
                    // Take the logarithm with the base TODO fast logarithm lookup, etc
                    magnitude = Math.log(magnitude) / logBase;
                }
                else {
                    magnitude /= base;
                }
                // Normalize to [minLightness, maxLightness), repeating
                magnitude = minLightness + rLightness * (magnitude - Math.floor(magnitude));
                writeHL(h, 4 / Math.PI * approxAtan(magnitude), arr, index);
            }
            if (isComplexArray(complexArr)) {
                for (let i = 0; i < complexArr.length; ++i) {
                    let c = complexArr[i];
                    let re = c.re, im = c.im;
                    write(re, im, index + (i << 2));
                }
            }
            else if (complexArr instanceof Float32Array) {
                // Split like this so that the stride is separate for typed arrays and hopefully won't cause deoptimization... lol
                for (let i = 0; i < complexArr.length; i += 2) {
                    let re = complexArr[i], im = complexArr[i + 1];
                    write(re, im, index + (i << 1));
                }
            }
            else {
                for (let i = 0; i < complexArr.length; i += 2) {
                    let re = complexArr[i], im = complexArr[i + 1];
                    write(re, im, index + (i << 1));
                }
            }
        }
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

    /**
     * Given two instruction lists, check whether they are the same based on versions
     * @param c1
     * @param c2
     */
    function differentInfo(c1, c2) {
        var _a, _b;
        if (!c1 || !c2)
            return c1 === c2; // undefined case
        let len;
        if ((len = c1.length) !== c2.length)
            return true;
        for (let i = 0; i < len; ++i) {
            let c1v = (_a = c1[i]) === null || _a === void 0 ? void 0 : _a.version;
            let c2v = (_b = c2[i]) === null || _b === void 0 ? void 0 : _b.version;
            if (!c1v || !c2v || c1v !== c2v)
                return true;
        }
        return false;
    }
    // Corresponds one-to-one with an Element
    class SceneAnnotatedNode {
        constructor() {
            this.rInfoVersion = -1;
            this.lastChildren = this.children = [];
            this.childrenOrderChanged = true;
        }
        markHistorical() {
            this.lastRInfo = this.rInfo;
            this.lastChildren = this.children;
        }
        markInfoProcessed() {
            this.rInfoChanged = true;
        }
        getVersion() {
            return this.rInfoVersion;
        }
        /**
         * Set the info directly, overriding whether the info has the same version as before
         * @param r
         */
        setInfo(r) {
            this.rInfo = r;
            if (r !== null && r.version !== undefined) {
                this.rInfoVersion = r.version;
            }
            else {
                this.rInfoVersion = getVersionID(); // auto generated
            }
            this.rInfoChanged = true;
        }
        setChildrenOrder(o) {
            this.lastChildren = this.children;
            this.children = o;
            this.childrenOrderChanged = true;
        }
        /**
         * Wheter the given rendering info is equivalent, based on version
         * @param r
         * @param checkDeep Whether to also compare versions, etc. of contexts
         * @return 00 if entirely the same, 01 if only instructions are different, 10 if only contexts are different, 11 if
         * everything is different. If checkDeep is turned off, only returns 00 or 11
         */
        differentRInfoAs(r, checkDeep = true) {
            let tr = this.rInfo;
            if (!tr || !r) {
                if (tr === r)
                    return 0b00;
                if (checkDeep)
                    return 0b11;
                return (+((r === null || r === void 0 ? void 0 : r.contexts) !== (tr === null || tr === void 0 ? void 0 : tr.contexts))) & (+((r === null || r === void 0 ? void 0 : r.instructions) !== (tr === null || tr === void 0 ? void 0 : tr.instructions)) << 1);
            }
            let versionsSame = r.version === this.getVersion();
            if (versionsSame)
                return 0b00;
            if (!checkDeep)
                return 0b11;
            return (+differentInfo(tr.contexts, r.contexts)) & ((+differentInfo(tr.instructions, r.instructions)) << 1);
        }
    }
    class SceneGraphPerfCounters {
        constructor() {
            this.clear();
        }
        clear() {
            this.removedElements = 0;
            this.buildCount = 0;
        }
    }
    class SceneGraphError extends Error {
        constructor(message) {
            super(message);
            this.name = 'SceneGraphError';
        }
    }
    class SceneGraph {
        constructor(renderer) {
            this.renderer = renderer;
            this.id = getStringID();
            this.sceneAnnotatedNodes = new Map();
            this.sceneTopNode = null;
            this.sceneNodeCount = -1;
            this.perfCounters = new SceneGraphPerfCounters();
        }
        /**
         * Whether this scene graph has been built from a scene
         */
        hasScene() {
            return !!this.sceneTopNode;
        }
        /**
         * Clear all caches, delete all buffer data
         */
        invalidateEverything() {
            this.sceneNodeCount = -1;
            this.sceneAnnotatedNodes.clear();
        }
        _getSceneRenderingInfo(scene) {
            // We traverse the scene, get rendering info, and compare it to our existing sceneAnnotatedNodes tree
            let processedID = getVersionID();
            let nodeCount = 0;
            scene.apply(sceneE => {
                let id = sceneE.id, parentId = sceneE.parent ? sceneE.parent.id : null;
                let annotated = this.sceneAnnotatedNodes.get(id);
                if (!annotated) {
                    // New node
                    annotated = new SceneAnnotatedNode();
                    annotated.id = id;
                    annotated.parentId = parentId;
                    this.sceneAnnotatedNodes.set(id, annotated);
                }
                // Changed parents are supposed to be handled in the parent's context
                if (annotated.parentId !== parentId) {
                    throw new SceneGraphError(`Parent id of annotated node is ${annotated.parentId}, should be ${parentId} and processed earlier`);
                }
                let children = sceneE.getChildren();
                let rInfo = sceneE.getRenderingInfo();
                // There are three jobs to do: compare contexts, compare renderer instructions, and compare children. Reordering
                // of children is handled specially, but reordering of the others is not (too much complexity!)
                let newRInfo = annotated.differentRInfoAs(rInfo);
                if (newRInfo) {
                    annotated.setInfo(rInfo);
                }
                if (children.length === 0 && annotated.children.length === 0) ;
                else {
                    // Ascertain children order and remove old children if necessary
                    let annotatedChildren = annotated.children;
                    let acl = annotatedChildren.length;
                    let cl = children.length;
                    let maxl = acl > cl ? acl : cl;
                    let orderChanged = false;
                    let newOrder = [];
                    // TODO optimize to prevent unncessary creation of strings when the order is unchanged
                    // Look at current children and compare to annotated children
                    for (let i = 0; i < maxl; ++i) {
                        let child = children[i];
                        let annotatedChildID = annotatedChildren[i];
                        if (!child) {
                            this._markSuspiciousChild(annotatedChildID, processedID);
                            orderChanged = true;
                            continue;
                        }
                        let childID = child.id;
                        newOrder.push(childID);
                        if (childID !== annotatedChildID) {
                            orderChanged = true;
                            // Potentially deleted or moved
                            this._markSuspiciousChild(annotatedChildID, processedID);
                        }
                        let existingAnnotatedChild = this.sceneAnnotatedNodes.get(childID);
                        if (existingAnnotatedChild && existingAnnotatedChild.parentId !== id) {
                            // Change parent!
                            existingAnnotatedChild.parentId = id;
                        }
                    }
                    if (orderChanged) {
                        annotated.setChildrenOrder(newOrder);
                    }
                }
                annotated.processedID = processedID; // mark node as processed in this pass
                nodeCount++;
            });
            this._removeUnaccountedChildren(processedID);
            this.sceneNodeCount = nodeCount;
        }
        /**
         * Traverses nodes in order
         */
        forEachAnnotatedNode(callback) {
            let topID = this.sceneTopNode;
            if (!topID)
                return; // nothing to traverse
            let recurse = (id) => {
                let node = this.sceneAnnotatedNodes.get(id);
                if (!node)
                    throw new SceneGraphError("??");
                callback(node);
                for (let id of node.children) {
                    recurse(id);
                }
            };
            recurse(topID);
        }
        /**
         * For debugging only
         */
        _flattenedAnnotatedGraphIDs() {
            let arr = [];
            this.forEachAnnotatedNode(n => arr.push(n.id));
            return arr;
        }
        /**
         * Child may have been moved or deleted; mark it as -1
         * @param id
         */
        _markSuspiciousChild(id, processedID) {
            let c = this.sceneAnnotatedNodes.get(id);
            if (c && c.processedID !== processedID /* hasn't been processed previously */)
                c.processedID = -1;
        }
        _removeUnaccountedChildren(processedID) {
            let annotatedN = this.sceneAnnotatedNodes;
            for (let key of annotatedN.keys()) {
                let node = annotatedN.get(key);
                let nID = node.processedID;
                if (nID === -1) {
                    // Deleted, unclaimed child
                    this.perfCounters.removedElements++;
                }
                else if (nID !== processedID) {
                    // Node has been deleted, which should have been detected! throw an error
                    throw new SceneGraphError(`Unaccounted node ${node.id} (parent ${node.parentId}) still exists in scene graph`);
                }
            }
        }
        /**
         * Construct the scene graph from a scene by calling getRenderingInfo on all children, then performing an ordering
         * @param scene If null, the graph is cleared
         */
        buildFromScene(scene) {
            this.perfCounters.buildCount++;
            let sceneID = scene === null || scene === void 0 ? void 0 : scene.id;
            if (sceneID !== this.sceneTopNode) { // The scene has changed, invalidate everything
                this.sceneTopNode = sceneID !== null && sceneID !== void 0 ? sceneID : null;
                this.invalidateEverything();
            }
            if (!scene)
                return;
            this._getSceneRenderingInfo(scene);
        }
        // Sort and shift around instructions based on their zIndices and escapeContext status
        orderInstructions() {
        }
    }

    /**
     * Generic Vec2 class, reinventing the wheel...
     */
    class Vec2 {
        constructor(x, y) {
            this.x = x;
            this.y = y;
        }
        /**
         * Permissively convert to Vec2 from an arbitrary object, ideally something that is Vec2Like
         * @param obj
         */
        static fromObj(obj) {
            let x = NaN, y = NaN;
            if (Array.isArray(obj)) {
                x = obj[0];
                y = obj[1];
            }
            else if (typeof obj === 'object' && (obj !== null)) {
                let o = obj;
                if ('x' in obj) {
                    x = +o.x;
                    y = +o.y;
                }
                else if ('re' in obj) {
                    x = +o.re;
                    y = +o.im;
                }
            }
            else if (typeof obj === 'string') {
                switch (obj) {
                    case 'N':
                    case 'NE':
                    case 'NW':
                        y = 1;
                        break;
                    case 'S':
                    case 'SE':
                    case 'SW':
                        y = -1;
                        break;
                    case 'C':
                        y = 0;
                }
                switch (obj) {
                    case 'E':
                    case 'NE':
                    case 'SE':
                        x = 1;
                        break;
                    case 'W':
                    case 'NW':
                    case 'SW':
                        x = -1;
                        break;
                    case 'C':
                        x = 0;
                }
            }
            return new Vec2(x, y);
        }
        add(vec) {
            return new Vec2(this.x + vec.x, this.y + vec.y);
        }
        subtract(vec) {
            return new Vec2(this.x - vec.x, this.y - vec.y);
        }
        multiplyScalar(scalar) {
            return new Vec2(this.x * scalar, this.y * scalar);
        }
        rot(angle, centre) {
            // TODO
            let s = Math.sin(angle), c = Math.cos(angle);
            if (!centre)
                return new Vec2(c * this.x - s * this.y, s * this.x + c * this.y);
        }
        rotDeg(angle, centre) {
            return this.rot((angle * Math.PI) / 180, centre);
        }
        unit() {
            return this.multiplyScalar(1 / this.length());
        }
        length() {
            return Math.hypot(this.x, this.y);
        }
        lengthSquared() {
            return this.x * this.x + this.y * this.y;
        }
    }
    function throwInconvertibleElement(elem, i) {
        throw new TypeError(`Expected input to be convertible to a flat array of Vec2s, found element ${elem} at index ${i}`);
    }
    function throwOddLengthArray(len) {
        throw new Error(`Expected input to be convertible to a flat array of Vec2s, got array of odd length ${len}`);
    }
    function vec2NonFlatArrayConversion(arr, f32 = true, throwOnError = false) {
        let ret = new (f32 ? Float32Array : Float64Array)(arr.length * 2);
        let retIndex = -1;
        for (let i = 0; i < arr.length; ++i) {
            let elem = arr[i];
            if (elem.x) {
                ret[++retIndex] = elem.x;
                ret[++retIndex] = elem.y;
            }
            else if (Array.isArray(elem)) {
                if (elem.length !== 2) {
                    if (throwOnError)
                        throwInconvertibleElement(elem, i);
                    return;
                }
                ret[++retIndex] = elem[0];
                ret[++retIndex] = elem[1];
            }
            else {
                if (throwOnError)
                    throwInconvertibleElement(elem, i);
                return;
            }
        }
        return ret;
    }
    /**
     * Flatten a given array relatively permissively into an flat array; returns undefined if the conversion failed
     * @param obj
     * @param f32
     * @param throwOnError
     */
    function vec2ArrayConversion(obj, f32 = true, throwOnError = false) {
        if (Array.isArray(obj)) {
            for (let i = 0; i < obj.length; ++i) {
                if (typeof obj[i] !== 'number') {
                    return vec2NonFlatArrayConversion(obj);
                }
            }
            // Obj is just an array of numbers
            if (obj.length % 2 === 1) {
                if (throwOnError)
                    throwOddLengthArray(obj.length);
                return;
            }
            return new (f32 ? Float32Array : Float64Array)(obj);
        }
        else if (isTypedArray(obj)) {
            if (obj.length % 2 === 1) {
                if (throwOnError)
                    throwOddLengthArray(obj.length);
                return;
            }
            if (f32 && obj instanceof Float32Array)
                return obj;
            if (!f32 && obj instanceof Float64Array)
                return obj;
            return new (f32 ? Float32Array : Float64Array)(obj);
        }
        if (throwOnError)
            throw new TypeError(`Could not convert object into flat Vec2 array`);
    }

    /**
     * Here lies madness.
     *
     * Grapheme's renderer is going to be pretty monolithic, with a lot of interdependent moving parts. As such, I'm going
     * to keep it mostly contained within one class, perhaps with some helper classes. Doing so will also help eliminate
     * fluff and make optimization easy and expressive. In any case, the renderer is effectively a state machine of various
     * draw calls. The final call to copy the internal buffer to the screen uses transferFromImageBitmap, which is
     * asynchronous and thus allows WebGL to proceed in a a separate thread—as it is implemented by modern browsers.
     *
     * On the surface, Grapheme's rendering sequence is simple: the renderer traverses through the scene, calls
     * getRenderingInfo() on every element, compiles a list of all the instructions (which look something like
     * "draw this set of triangles", "draw this text"), and runs them all, returning the final product. But if the rendering
     * pipeline were so simple, there would be little point in using WebGL at all. Why not just use Canvas2D? Why learn such
     * a painful API? The name of the game is parallelism and optimization. Where WebGL excels is low-level control
     * and rapid parallel computation. Its weaknesses are in a lack of builtin functions (lacking text, for example) and
     * high complexity and verbosity,
     *
     * Imagine we did indeed render a scene instruction by instruction. We come across a line, so we switch to the polyline
     * program, load in the vertices into a buffer, and drawArrays -- draw it to the canvas. We then come across a piece of
     * text. WebGL cannot render text, so we switch over to a Canvas2D context and draw a piece of text onto a blank canvas.
     * We then load the blank canvas as a texture into WebGL and switch to the text program, loading in a set of vertices
     * specifying where the text is, and calling drawArrays. We then come across a couple hundred polylines in a row. For
     * each polyline, we copy its data to the buffer and render it.
     *
     * There are two serious problems here. One is that loading buffers and textures is slow, for various
     * reasons. Another is that parallelism is seriously lacking. We have to call drawArrays several hundred times for those
     * polylines, and each call has a large constant overhead.
     *
     * The renderer thus has several difficult jobs: minimizing buffer and texture loading, and combining consecutive calls
     * into one large drawArrays call. Accomplishing these jobs (and a few more) requires somewhat intricate work,
     * which should of course be designed to allow more esoteric draw calls -- for a Mandelbrot set, say -- to still be
     * handled with consistency. There is no perfect solution, but there are certainly gains to be made. As with the props
     * of Grapheme elements, the problem is made easier by high-level abstraction. The renderer should produce a comparable
     * result when optimized, compared to when every call is made individually. (They need not be exactly the same, for
     * reasons that will become apparent.)
     *
     * Even more annoying is that the WebGL context may suddenly crash and all its buffers and programs lost in the ether.
     * The renderer thus has to be able to handle such data loss without indefinitely screwing up the rendering process. So
     * I have my work cut out, but that's exciting.
     *
     * The current thinking is a z-index based system with heuristic reallocation of changing and unchanging buffers. Given
     * a list of elements and each element's instructions, we are allowed to rearrange the instructions under certain
     * conditions: 1. instructions are drawn in order of z-index and 2. specific instructions within a given z-index may
     * specify that they must be rendered in the order in which they appear in the instruction list. The latter condition
     * allows deterministic ordering of certain instructions on the same z-index, which is useful when that suborder does
     * matter (like when two instructions for a given element are intended to be one on top of the other). Otherwise, the
     * instructions may be freely rearranged and (importantly) combined into larger operations that look the same.
     *
     * Already, such a sorting system is very helpful. Text elements generally specify a z-index of Infinity, while
     * gridlines might specify a z-index of 0 to be behind most things, and a draggable point might have an index of 20. A
     * simple algorithm to render a static image is to sort by z-index, then within each z-index group triangle draw calls
     * with the same color together, and group text draw calls together. We then proceed to render each z-index's grouped
     * calls in order.
     *
     * For a static scene, such a rendering system would work great. But in a dynamic scene, constantly reoptimizing the
     * entire scene as a result of changing some inconsequential little geometry would be stupid. Ideally, changing a little
     * geometry would merely update a single buffer or subsection of a buffer. Yet some changes do require a complete re-
     * distribution of instructions; if the scene's size doubled, for example, and all the elements changed substantially.
     * We can certainly cache information from the previous rendering process of a scene, but what do we cache? How do we
     * ensure stability and few edge cases? How do we deal with context loss?
     *
     * The first step is to understand exactly what instructions are. *Anonymous* instructions have a type, some data, and
     * an element id (which element it originated from). *Normal* instructions have a type, some data, an element id, an
     * instruction id, and a version. The point of normal instructions is to represent a sort of "draw concept", where after
     * an update, that instruction may have changed slightly, but will still have the same id. The instruction associated
     * with a function plot, for example, will have some numerical ID, and when the plot changes somehow, the version will
     * increase, but the numerical ID will remain the same. Conceptually, this means that the instruction to draw the
     * function plot has been rewritten, and the old data is basically irrelevant -- and buffers associated with that
     * data can and should be reused or reallocated.
     *
     * Anonymous instructions, on the other hand, have no concept of "versioning". Anonymous instructions are
     * entirely reallocated or deleted every time their element updates. These instructions are generally used to indicate
     * instructions which are very prone to change and where its values should be tied solely to the element updating.
     */
    // Functions taken from Mozilla docs
    function createShaderFromSource(gl, shaderType, shaderSource) {
        var _a;
        const shader = gl.createShader(shaderType);
        let err;
        if (!shader) {
            err = "Failed to create shader";
        }
        else {
            gl.shaderSource(shader, shaderSource);
            gl.compileShader(shader);
            const succeeded = gl.getShaderParameter(shader, gl.COMPILE_STATUS);
            if (succeeded)
                return shader;
            err = ((_a = gl.getShaderInfoLog(shader)) !== null && _a !== void 0 ? _a : "Failed to create shader or get info log");
        }
        gl.deleteShader(shader);
        throw new Error("createShaderFromSource: " + err);
    }
    function createGLProgram(gl, vertexShader, fragShader) {
        var _a;
        const program = gl.createProgram();
        let err;
        if (!program) {
            err = "Failed to create program";
        }
        else {
            gl.attachShader(program, vertexShader);
            gl.attachShader(program, fragShader);
            gl.linkProgram(program);
            const succeeded = gl.getProgramParameter(program, gl.LINK_STATUS);
            if (succeeded)
                return program;
            err = (_a = gl.getProgramInfoLog(program)) !== null && _a !== void 0 ? _a : "Failed to create program or get info log-";
        }
        gl.deleteProgram(program);
        throw new Error("createGLProgram: " + err);
    }
    class WebGLRenderer {
        constructor() {
            const canvas = document.createElement('canvas');
            const gl = canvas.getContext('webgl2');
            if (!gl) {
                throw new Error("WebGL2 not supported");
            }
            this.canvas = canvas;
            this.gl = gl;
            /**
             * Map between scene ids and known information about them
             * @type {Map<string, {}>}
             */
            this.sceneCaches = new Map();
            this.programs = new Map();
            this.buffers = new Map();
            this.textures = new Map();
            this.vaos = new Map();
        }
        /**
         * Create and link a program and store it in the form { glProgram, attribs, uniforms }, where glProgram is the
         * underlying program and attributes and uniforms are a dictionary of attributes and uniforms from the program. The
         * attributes are given as an object, of manually assigned indices; these are required and will not be assigned
         * automatically. Returns null on failure—does not throw
         * @param programName
         * @param vertexShaderSource
         * @param fragShaderSource
         * @param attributeBindings
         * @param uniformNames
         * @return GLProgramStore
         */
        createProgram(programName, vertexShaderSource, fragShaderSource, attributeBindings, uniformNames) {
            this.deleteProgram(programName);
            const { gl } = this;
            const glProgram = createGLProgram(gl, createShaderFromSource(gl, gl.VERTEX_SHADER, vertexShaderSource), createShaderFromSource(gl, gl.FRAGMENT_SHADER, fragShaderSource));
            for (let name in attributeBindings) {
                let loc = attributeBindings[name];
                gl.bindAttribLocation(glProgram, loc, name);
            }
            const uniforms = {};
            for (const name of uniformNames) {
                let loc = gl.getUniformLocation(glProgram, name);
                if (!loc) {
                    gl.deleteProgram(glProgram);
                    throw new Error(`Unable to find uniform ${name}`);
                }
                uniforms[name] = loc;
            }
            const program = { program: glProgram, attributes: attributeBindings, uniforms };
            this.programs.set(programName, program);
            return program;
        }
        /**
         * Get the program of a given name, returning undefined if it does not exist
         * @param programName
         * @returns
         */
        getProgram(programName) {
            var _a;
            return (_a = this.programs.get(programName)) !== null && _a !== void 0 ? _a : null;
        }
        /**
         * Delete a program, including the underlying GL program
         * @param programName {string}
         */
        deleteProgram(programName) {
            const program = this.getProgram(programName);
            if (program) {
                this.gl.deleteProgram(program.program);
                this.programs.delete(programName);
            }
        }
        getTexture(textureName) {
            var _a;
            return (_a = this.textures.get(textureName)) !== null && _a !== void 0 ? _a : null;
        }
        deleteTexture(textureName) {
            let texture = this.getTexture(textureName);
            if (texture !== undefined) {
                this.gl.deleteTexture(this.getTexture(textureName));
                this.textures.delete(textureName);
            }
        }
        createTexture(textureName) {
            this.deleteTexture(textureName);
            const texture = this.gl.createTexture();
            if (!texture)
                return null;
            const store = { texture };
            this.textures.set(textureName, store);
            return store;
        }
        getBuffer(bufferName) {
            var _a;
            return (_a = this.buffers.get(bufferName)) !== null && _a !== void 0 ? _a : null;
        }
        /**
         * Create a buffer with a given name, returning the existing buffer if one exists
         * @param bufferName
         */
        createBuffer(bufferName) {
            let buffer = this.getBuffer(bufferName);
            if (!buffer) {
                let glBuffer = this.gl.createBuffer();
                if (!glBuffer)
                    return null;
                buffer = { buffer: glBuffer };
                this.buffers.set(bufferName, buffer);
            }
            return buffer;
        }
        deleteBuffer(bufferName) {
            const buffer = this.getBuffer(bufferName);
            if (buffer) {
                this.buffers.delete(bufferName);
                this.gl.deleteBuffer(buffer);
            }
        }
        getVAO(vaoName) {
            var _a;
            return (_a = this.vaos.get(vaoName)) !== null && _a !== void 0 ? _a : null;
        }
        createVAO(vaoName) {
            let vao = this.getVAO(vaoName);
            if (!vao) {
                let glVao = this.gl.createVertexArray();
                if (!glVao)
                    return null;
                this.vaos.set(vaoName, { vao: glVao });
            }
            return vao;
        }
        deleteVAO(vaoName) {
            const vao = this.getVAO(vaoName);
            if (vao) {
                this.vaos.delete(vaoName);
                this.gl.deleteVertexArray(vao.vao);
            }
        }
        /**
         * Resize and clear the canvas simultaneously. The canvas is only manually cleared if the dimensions haven't changed,
         * since the buffer will be erased.
         * @param width
         * @param height
         * @param dpr
         * @param clear {Color}
         */
        clearAndResizeCanvas(width, height, dpr = 1, clear = Colors.TRANSPARENT) {
            const { canvas } = this;
            this.dpr = dpr;
            if (canvas.width === width && canvas.height === height) {
                // No need to reset the canvas
                this.clearCanvas(clear);
            }
            else {
                canvas.width = width;
                canvas.height = height;
                // If the given color is not plain transparent black, we need to set the canvas color directly
                if (!clear.equals(Colors.TRANSPARENT)) {
                    this.clearCanvas(clear);
                }
            }
            this.gl.viewport(0, 0, width, height);
        }
        /**
         * Clear the canvas to the given color
         * @param clearColor
         */
        clearCanvas(clearColor = Colors.TRANSPARENT) {
            const gl = this.gl;
            gl.clearColor(clearColor.r / 255, clearColor.g / 255, clearColor.b / 255, clearColor.a / 255);
            gl.clear(gl.COLOR_BUFFER_BIT);
        }
        /**
         * Overall scaling from pixel space to clip space ((-1,-1) bottom left; (1,1) top right). In other words, the distance
         * between two pixels horizontally is v.x in clip space, and vertically, v.y.
         */
        getXYScale() {
            let { canvas, dpr } = this;
            return new Vec2(2 / canvas.width * dpr, -2 / canvas.height * dpr);
        }
        renderDOMScene(scene) {
            let graph = new SceneGraph(this);
            window.graph = graph;
            graph.buildFromScene(scene);
            graph.assembleInstructions();
            graph.compile();
            createImageBitmap(this.canvas).then(bitmap => {
                console.log(bitmap);
                scene.bitmapRenderer.transferFromImageBitmap(bitmap);
            });
        }
    }

    // TODO
    /**
     * Given some parameters describing a line segment, find a line segment that is consistent with at least two of them.
     * @param x1 {number}
     * @param x2 {number}
     * @param w {number}
     * @param cx {number}
     */
    function resolveAxisSpecification(x1, x2, w, cx) {
        if (cx !== undefined) {
            let halfWidth = 0;
            if (w !== undefined)
                halfWidth = w / 2;
            else if (x2 !== undefined)
                halfWidth = x2 - cx;
            else if (x1 !== undefined)
                halfWidth = cx - x1;
            halfWidth = Math.abs(halfWidth);
            return [cx - halfWidth, cx + halfWidth];
        }
        else if (x1 !== undefined) {
            if (w !== undefined)
                return [x1, x1 + w];
            if (x2 !== undefined)
                return [x1, x2];
        }
        else if (x2 !== undefined) {
            if (w !== undefined)
                return [x2 - w, x2];
        }
        return [0, 0];
    }
    /**
     * A bounding box. In general, we consider the bounding box to be in canvas coordinates, so that the "top" is -y and
     * the "bottom" is +y.
     */
    class BoundingBox {
        constructor(x = 0, y = 0, width = 0, height = 0) {
            this.x = x;
            this.y = y;
            this.w = width;
            this.h = height;
        }
        clone() {
            return new BoundingBox(this.x, this.y, this.w, this.h);
        }
        /**
         * Push in (or pull out) all the sides of the box by a given amount. Returns null if too far. So squishing
         * { x: 0, y: 0, w: 2, h: 2} by 1/2 will give { x: 0.5, y: 0.5, w: 1, h: 1 }
         * @param margin {number}
         */
        squish(margin = 0) {
            const { x, y, w, h } = this;
            if (2 * margin > w || 2 * margin > h)
                return null;
            return new BoundingBox(x + margin, y + margin, w - 2 * margin, h - 2 * margin);
        }
        squishAsymmetrically(left = 0, right = 0, bottom = 0, top = 0, flipY = false) {
            const { x, y, w, h } = this;
            if (2 * (left + right) > w || 2 * (bottom + top) > h) {
                return null;
            }
            if (flipY) {
                let tmp = bottom;
                bottom = top;
                top = tmp;
            }
            return new BoundingBox(x + left, y + top, w - (left + right), h - (top + bottom));
        }
        translate(v) {
            return new BoundingBox(this.x + v.x, this.y + v.y, this.w, this.h);
        }
        scale(s) {
            return new BoundingBox(this.x * s, this.y * s, this.w * s, this.h * s);
        }
        getX2() {
            return this.x + this.w;
        }
        getY2() {
            return this.y + this.h;
        }
        /**
         * Attempt to convert an object to a bounding box via various interpretation methods. In particular, the relevant
         * parameters are x (or x1), y (or y1), x2, y2, w (or width), h (or height), cx (or centerX), and cy (or centerY).
         * @param obj
         */
        static fromObj(obj) {
            let finalX1, finalY1, finalX2, finalY2;
            if (Array.isArray(obj)) {
                finalX1 = obj[0];
                finalY1 = obj[1];
                finalX2 = obj[2] + finalX1;
                finalY2 = obj[3] + finalY1;
            }
            else if (typeof obj === 'object') {
                if (obj instanceof BoundingBox) {
                    return obj.clone();
                }
                let { x, y, x1, y1, x2, y2, w, h, width, height, cx, cy, centerX, centerY } = obj;
                // various aliases
                x = x !== null && x !== void 0 ? x : x1;
                y = y !== null && y !== void 0 ? y : y1;
                w = w !== null && w !== void 0 ? w : width;
                h = h !== null && h !== void 0 ? h : height;
                cx = cx !== null && cx !== void 0 ? cx : centerX;
                cy = cy !== null && cy !== void 0 ? cy : centerY;
                [finalX1, finalX2] = resolveAxisSpecification(x, x2, w, cx);
                [finalY1, finalY2] = resolveAxisSpecification(y, y2, h, cy);
            }
            finalX1 = +finalX1;
            finalY1 = +finalY1;
            finalX2 = +finalX2;
            finalY2 = +finalY2;
            if (finalX1 !== finalX1 || finalY1 !== finalY1 || finalX2 !== finalX2 || finalY2 !== finalY2) {
                throw new Error('BoundingBox.fromObj: Invalid bounding box specification');
            }
            return new BoundingBox(finalX1, finalY1, finalX2 - finalX1, finalY2 - finalY1);
        }
        get x1() {
            return this.x;
        }
        get y1() {
            return this.y;
        }
        get x2() {
            return this.getX2();
        }
        get y2() {
            return this.getY2();
        }
        tl() {
            return new Vec2(this.x, this.y);
        }
    }
    BoundingBox.Transform = Object.freeze({
        X: (x, box1, box2, flipX) => {
            if (Array.isArray(x) || isTypedArray(x)) {
                x = x;
                for (let i = 0; i < x.length; ++i) {
                    let fractionAlong = (x[i] - box1.x) / box1.w;
                    if (flipX)
                        fractionAlong = 1 - fractionAlong;
                    x[i] = fractionAlong * box2.w + box2.x;
                }
                return x;
            }
            else {
                x = x;
                return BoundingBox.Transform.X([x], box1, box2, flipX)[0];
            }
        },
        Y: (y, box1, box2, flipY) => {
            if (Array.isArray(y) || isTypedArray(y)) {
                y = y;
                for (let i = 0; i < y.length; ++i) {
                    let fractionAlong = (y[i] - box1.y) / box1.h;
                    if (flipY)
                        fractionAlong = 1 - fractionAlong;
                    y[i] = fractionAlong * box2.h + box2.y;
                }
                return y;
            }
            else {
                y = y;
                return BoundingBox.Transform.Y([y], box1, box2, flipY)[0];
            }
        },
        XY: (xy, box1, box2, flipX, flipY) => {
            if (Array.isArray(xy) || isTypedArray(xy)) {
                xy = xy;
                for (let i = 0; i < xy.length; i += 2) {
                    let fractionAlong = (xy[i] - box1.x) / box1.w;
                    if (flipX)
                        fractionAlong = 1 - fractionAlong;
                    xy[i] = fractionAlong * box2.w + box2.x;
                    fractionAlong = (xy[i + 1] - box1.y) / box1.h;
                    if (flipY)
                        fractionAlong = 1 - fractionAlong;
                    xy[i + 1] = fractionAlong * box2.h + box2.y;
                }
                return xy;
            }
            else {
                throw new TypeError("BoundingBox.Transform.XY only accepts numeric arrays (Arrays of numbers and typed arrays)");
            }
        },
        getReducedTransform(box1, box2, flipX, flipY) {
            let xm = 1 / box1.w;
            let xb = -box1.x / box1.w;
            if (flipX) {
                xm *= -1;
                xb = 1 - xb;
            }
            xm *= box2.w;
            xb *= box2.w;
            xb += box2.x;
            let ym = 1 / box1.h;
            let yb = -box1.y / box1.h;
            if (flipY) {
                ym *= -1;
                yb = 1 - yb;
            }
            ym *= box2.h;
            yb *= box2.h;
            yb += box2.y;
            return { xm, xb, ym, yb };
        }
    });

    class LinearPlot2DTransform {
        /**
         * Parameters beginning with p are the bounding box in pixel coordinates. Those beginning with g are the bounding box
         * in graph coordinates. The transform has an implicit y flipping operation, which is key. The point (px1, py1) does
         * NOT map to the point (gx1, gy1), but the point (gx1, gy1 + gh). This annoyance is why a special class is useful.
         * The default constructor maps to the unit square (ignoring any aspect ratio concerns)
         */
        constructor(px1 = 0, py1 = 0, pw = 400, ph = 400, gx1 = -1, gy1 = -1, gw = 2, gh = 2) {
            this.px1 = px1;
            this.py1 = py1;
            this.pw = pw;
            this.ph = ph;
            this.gx1 = gx1;
            this.gy1 = gy1;
            this.gw = gw;
            this.gh = gh;
        }
        get px2() {
            return this.px1 + this.pw;
        }
        get py2() {
            return this.py1 + this.ph;
        }
        get gx2() {
            return this.gx1 + this.gw;
        }
        get gy2() {
            return this.gy1 + this.gh;
        }
        pixelBox() {
            return new BoundingBox(this.px1, this.py1, this.pw, this.ph);
        }
        graphBox() {
            return new BoundingBox(this.gx1, this.gy1, this.gw, this.gh);
        }
        /**
         * Graph aspect ratio of this plot, defined by the ratio in pixels between a width and height of a unit square
         */
        getGraphAspectRatio() {
            return this.gw / this.pw * this.ph / this.gh;
        }
        resizeToPixelBox(box) {
            let convertedBox = BoundingBox.fromObj(box);
            this.px1 = convertedBox.x;
            this.py1 = convertedBox.y;
            this.pw = convertedBox.w;
            this.ph = convertedBox.h;
            return this;
        }
        resizeToGraphBox(box) {
            let convertedBox = BoundingBox.fromObj(box);
            this.gx1 = convertedBox.x;
            this.gy1 = convertedBox.y;
            this.gw = convertedBox.w;
            this.gh = convertedBox.h;
            return this;
        }
        setGraphXBounds(x1, x2) {
            this.gx1 = x1;
            this.gw = x2 - x1;
        }
        setGraphYBounds(y1, y2) {
            this.gy1 = y1;
            this.gh = y2 - y1;
        }
        setGraphXYBounds(x1, x2, y1, y2) {
            this.setGraphXBounds(x1, x2);
            this.setGraphYBounds(y1, y2);
        }
        clone() {
            return new LinearPlot2DTransform(this.px1, this.py1, this.pw, this.ph, this.gx1, this.gy1, this.gw, this.gh);
        }
        pixelToGraphX(x) {
            // This is not flipped
            return ((x - this.px1) / this.pw) * this.gw + this.gx1;
        }
        pixelToGraphY(y) {
            // This is flipped
            return (1 - (y - this.py1) / this.ph) * this.gh + this.gy1;
        }
        graphToPixelX(x) {
            // This is not flipped
            return ((x - this.gx1) / this.gw) * this.pw + this.px1;
        }
        graphToPixelY(y) {
            // This is flipped
            return (1 - (y - this.gy1) / this.gh) * this.ph + this.py1;
        }
        pixelToGraph(vec) {
            return new Vec2(this.pixelToGraphX(vec.x), this.pixelToGraphY(vec.y));
        }
        graphToPixel(vec) {
            return new Vec2(this.graphToPixelX(vec.x), this.graphToPixelY(vec.y));
        }
        /**
         * Return {xm, ym, xb, yb} where xm * x + xb is the transformation from graph x to pixel x, etc.
         */
        getReducedGraphToPixelTransform() {
            const { px1, py1, pw, ph, gx1, gy1, gw, gh } = this;
            return {
                xm: pw / gw,
                ym: -ph / gh,
                xb: (-gx1 / gw) * pw + px1,
                yb: (1 + gy1 / gh) * ph + py1
            };
        }
        graphToPixelArrInPlace(arr) {
            let { xm, ym, xb, yb } = this.getReducedGraphToPixelTransform();
            for (let i = 0; i < arr.length; i += 2) {
                arr[i] = xm * arr[i] + xb;
                arr[i + 1] = ym * arr[i + 1] + yb;
            }
            return arr;
        }
    }

    /**
     * Describes how a line should be drawn. Opacity should be set through color and is not tracked separately.
     */
    class Pen {
        constructor() {
        }
        static compose(...args) {
            let p = Pen.default();
            // Later arguments are given more precedence
            for (let spec of args) {
                if (spec.color !== undefined)
                    p.color = Color.fromObj(spec.color);
                if (spec.thickness !== undefined)
                    p.thickness = spec.thickness;
                if (spec.dashPattern !== undefined)
                    p.dashPattern = spec.dashPattern;
                if (spec.dashOffset !== undefined)
                    p.dashOffset = spec.dashOffset;
                if (spec.endcap !== undefined)
                    p.endcap = spec.endcap;
                if (spec.endcapRes !== undefined)
                    p.endcapRes = spec.endcapRes;
                if (spec.join !== undefined)
                    p.join = spec.join;
                if (spec.joinRes !== undefined)
                    p.joinRes = spec.joinRes;
                if (spec.useNative !== undefined)
                    p.useNative = spec.useNative;
                if (spec.visible !== undefined)
                    p.visible = spec.visible;
            }
            p.color = p.color.clone();
            return p;
        }
        static create(params) {
            return Pen.compose(params);
        }
        static default() {
            let p = new Pen();
            p.color = new Color(0, 0, 0, 255);
            p.thickness = 2;
            p.dashPattern = [];
            p.dashOffset = 0;
            p.endcap = 'round';
            p.endcapRes = 1;
            p.join = 'dynamic';
            p.joinRes = 1;
            p.useNative = false;
            p.visible = true;
            return p;
        }
        static fromObj(o) {
            if (typeof o === 'string')
                return _interpretStringAsPen(o);
            return Pen.compose(Pen.default(), o);
        }
        _toJoinTypeEnum() {
            return Pen.JOIN_TYPES[this.join];
        }
        _toEndcapTypeEnum() {
            return Pen.ENDCAP_TYPES[this.endcap];
        }
        equals(p) {
            return deepEquals(this, p);
        }
    }
    // Enum for endcaps
    Pen.ENDCAP_TYPES = Object.freeze({
        butt: 0,
        round: 1,
        square: 2
    });
    // Enum for join types
    Pen.JOIN_TYPES = Object.freeze({
        bevel: 0,
        miter: 2,
        round: 1,
        dynamic: 3
    });
    // TODO
    function _interpretStringAsPen(str) {
        try {
            let color = Color.fromCss(str);
            return Pen.fromObj({ color });
        }
        catch (_a) {
            return Pen.default();
        }
    }

    /**
     * A base class to use for event listeners and the like. Supports things like addEventListener(eventName, callback),
     * triggerEvent(name, ?data), removeEventListener( ... ), removeEventListeners(?name). Listeners are called with
     * data and this as parameters. If a listener returns true, the event does not propagate to any other listeners.
     */
    class Eventful {
        constructor() {
            this.eventListeners = new Map();
        }
        /**
         * Register an event listener to a given event name. It will be given lower priority than the ones that came before.
         * The callbacks will be given a single parameter "data".
         * @param eventName The name of the event
         * @param callback The callback(s) to register
         * @returns Returns self (for chaining)
         */
        addEventListener(eventName, callback) {
            if (Array.isArray(callback)) {
                for (const c of callback)
                    this.addEventListener(eventName, c);
                return this;
            }
            else if (typeof callback === 'function') {
                if (typeof eventName !== 'string' || !eventName)
                    throw new TypeError('Invalid event name');
                let listeners = this.eventListeners.get(eventName);
                if (!listeners) {
                    listeners = [];
                    this.eventListeners.set(eventName, listeners);
                }
                if (!listeners.includes(callback))
                    listeners.push(callback);
                return this;
            }
            else
                throw new TypeError('Invalid callback');
        }
        /**
         * Get the event listeners under "eventName", cloned so that they can be derped around with
         * @param eventName {string} Name of the event whose listeners we want
         * @returns {Array<function>}
         */
        getEventListeners(eventName) {
            const listeners = this.eventListeners.get(eventName);
            return Array.isArray(listeners) ? listeners.slice() : [];
        }
        /**
         * Whether there are any event listeners registered for the given name
         * @param eventName
         * @returns {boolean} Whether any listeners are registered for that event
         */
        hasEventListenersFor(eventName) {
            return Array.isArray(this.eventListeners.get(eventName));
        }
        /**
         * Remove an event listener from the given event. Fails silently if that listener is not registered.
         * @param eventName {string} The name of the event
         * @param callback {function} The callback to remove
         * @returns {Eventful} Returns self (for chaining)
         */
        removeEventListener(eventName, callback) {
            if (Array.isArray(callback)) {
                for (const c of callback)
                    this.removeEventListener(eventName, c);
                return this;
            }
            const listeners = this.eventListeners.get(eventName);
            if (Array.isArray(listeners)) {
                const index = listeners.indexOf(callback);
                if (index !== -1)
                    listeners.splice(index, 1);
                if (listeners.length === 0)
                    this.eventListeners.delete(eventName);
            }
            return this;
        }
        /**
         * Remove all event listeners for a given event. Fails silently if there are no listeners registered for that event.
         * @param eventName {string} The name of the event whose listeners should be cleared
         * @returns {Eventful} Returns self (for chaining)
         */
        removeEventListeners(eventName) {
            this.eventListeners.delete(eventName);
            return this;
        }
        /**
         * Trigger the listeners registered under an event name, passing (data, this, eventName) to each. Returns true if
         * some listener returned true, stopping propagation; returns false otherwise
         * @param eventName {string} Name of the event to be triggered
         * @param data {any} Optional data parameter to be passed to listeners
         * @returns {boolean} Whether any listener stopped propagation
         */
        triggerEvent(eventName, data) {
            if (this.eventListeners.size === 0)
                return false;
            const listeners = this.eventListeners.get(eventName);
            if (Array.isArray(listeners)) {
                for (let i = 0; i < listeners.length; ++i) {
                    if (listeners[i](data))
                        return true;
                }
            }
            return false;
        }
    }

    class Props {
        constructor() {
            this.store = new Map();
            this.proxy = new Proxy(this, proxyHandlers);
            this.hasChangedProperties = 0;
            this.hasChangedInheritableProperties = 0;
        }
        static toBit(as) {
            switch (as) {
                case 'program':
                    return 2;
                case 'user':
                    return 1;
                case 'real':
                case 'default':
                    return 0;
            }
            throw new Error(`Unknown accessor name ${as}`);
        }
        _getPropertyStore(propName) {
            return this.store.get(propName);
        }
        _setPropertyStore(propName, value) {
            this.store.set(propName, value);
        }
        /**
         * Create a property store for a given prop, returning the store. It returns the already-existing store, if appropriate.
         * @param propName
         * @returns Property store associated with the given property name
         */
        _createPropertyStore(propName) {
            let existing = this._getPropertyStore(propName);
            if (!existing) {
                existing = { value: undefined, changed: 0 };
                this._setPropertyStore(propName, existing);
            }
            return existing;
        }
        /**
         * Deletes a property store wholesale, not trying to account for changed values and the like.
         * @param propName
         */
        _deletePropertyStore(propName) {
            this.store.delete(propName);
        }
        _forEachStore(callback) {
            for (let value of this.store.values()) {
                callback(value);
            }
        }
        /**
         * Call the function with the name and (real) value of each property
         * @param callback
         */
        forEachProperty(callback) {
            for (let [key, value] of this.store.entries()) {
                callback(key, value);
            }
        }
        /**
         * Get a list of all properties, including ones which are undefined but still have a store
         */
        listProperties() {
            return Array.from(this.store.keys());
        }
        /**
         * Returns whether a property has changed relative to the last marked update
         * @param propName
         */
        hasChanged(propName) {
            var _a;
            return !!((_a = this._getPropertyStore(propName)) === null || _a === void 0 ? void 0 : _a.changed);
        }
        /**
         * Returns whether any property of a list of properties has changed, locally speaking.
         * @param propList
         */
        haveChanged(propList) {
            return (!!this.hasChangedProperties && propList.some(prop => this.hasChanged(prop)));
        }
        /**
         * Returns whether a given property is inheritable (i.e., an inherit value of 1 or 2).
         * @param propName
         * @returns
         */
        isPropertyInheritable(propName) {
            var _a;
            return !!((_a = this._getPropertyStore(propName)) === null || _a === void 0 ? void 0 : _a.inherit);
        }
        /**
         * Returns a list of properties which have changed, locally speaking.
         */
        listChangedProperties() {
            return this.listProperties().filter(prop => this.hasChanged(prop));
        }
        /**
         * Returns a list of properties which are to be inherited (i.e., an inherit of 1 or 2).
         */
        listInheritableProperties() {
            return this.listProperties().filter(prop => this.isPropertyInheritable(prop));
        }
        /**
         * Inherit all inheritable properties from a given props. The function does this by comparing the local inherited
         * prop's version to the given props's version. If the local version is lower, the property and version are copied,
         * and the changed status is set to true. If updateAll is set to true, the function makes sure to check that the
         * actual list of inherited properties is synchronized, because it normally only checks the local inheritable
         * properties and compares them. In fact, it only checks the local inheritable properties with inherit: 1, which
         * indicates it came from a parent rather than being defined in the current element.
         * @param props {Props}
         * @param updateAll {boolean} Whether to force a complete update, in which the inheritable properties are verifiably
         * synced with the top element's properties. This usually happens after an element is added to a group, or after a
         * group's inheritance signature has changed.
         */
        inheritPropertiesFrom(props, updateAll = false) {
            // Early exit condition, where if no inheritable properties have changed, we need not do anything
            if (!(updateAll || props.hasChangedInheritableProperties))
                return;
            updateAll = updateAll || props.hasChangedInheritableProperties === 2;
            // We recalculate all local properties whose inheritance is 1, indicating they were inherited from above. Properties
            // not found above are deleted, properties found above are copied if their version is greater than or equal to the
            // version of the current property. This ensures that this props does not have any extraneous properties or any
            // incorrect/nonupdated values.
            for (const [propName, propStore] of this.store.entries()) {
                if (propStore.inherit !== 1)
                    continue;
                const otherPropsStore = props._getPropertyStore(propName);
                // if no such inheritable property, delete the local property (do not keep it as inheritable)
                if (!otherPropsStore ||
                    +otherPropsStore.inherit < 1 ||
                    otherPropsStore.value === undefined) {
                    propStore.value = undefined;
                    propStore.changed |= 0b1;
                    propStore.inherit = 0;
                    this.markHasChangedProperties();
                    this.markHasChangedInheritableProperties();
                    continue;
                }
                // Value has been changed!
                if (propStore.version === undefined || (otherPropsStore.version > propStore.version)) {
                    propStore.version = otherPropsStore.version;
                    propStore.value = otherPropsStore.value;
                    propStore.changed |= 0b1;
                    this.markHasChangedProperties();
                    this.markHasChangedInheritableProperties();
                }
            }
            // If updateAll is true, we run through all the given properties and inherit all 1s and 2s.
            if (updateAll) {
                for (const [propName, propStore] of props.store.entries()) {
                    if (!propStore.inherit || propStore.value === undefined)
                        continue;
                    let ourPropStore = this._getPropertyStore(propName);
                    // Where things are actually inherited!!
                    if (!ourPropStore ||
                        (ourPropStore.inherit === 1 &&
                            (ourPropStore.version === undefined || propStore.version > ourPropStore.version))) {
                        if (!ourPropStore) {
                            ourPropStore = this._createPropertyStore(propName);
                            // Goes around set
                            ourPropStore.inherit = 1;
                            ourPropStore.value = propStore.value;
                            this.markHasChangedInheritanceSignature();
                        }
                        ourPropStore.version = propStore.version;
                        ourPropStore.value = propStore.value;
                        ourPropStore.changed |= 0b1;
                        this.markHasChangedProperties();
                    }
                }
            }
        }
        /**
         * This function sets the value of a property. It is meant mostly for internal use. If prompted, it will check to see
         * whether the value given and the current value are strictly equal, or deeply equal, and if so, not mark the property
         * as changed. By default, this check is turned off, meaning all value assignments are marked as "changed". The third
         * parameter indicates whether the value should be directly modified, or
         * @param propName {string} The name of the property
         * @param value {any} The value of the property
         * @param as {number} Which value to change. 0 if real, 1 if user, 2 if program
         * @param equalityCheck {number} What type of equality check to perform against the current value, if any, to assess
         * the changed value. 0 - no check, 1 - strict equals, 2 - deep equals, 3 - deep equals, looking for "equals()" methods
         * @param markChanged {boolean} Whether to actually mark the value as changed. In turn, if the property is a changed
         * inheritable property, that will be noted
         * @returns {any}
         */
        set(propName, value, as = 0, equalityCheck = 0, markChanged = true) {
            let store = this._getPropertyStore(propName);
            // Helper functions to abstract away the "user/program/real" concept
            function getStoreValue() {
                store = store;
                switch (as) {
                    case 0:
                        return store.value;
                    case 1:
                        return store.userValue;
                    case 2:
                        return store.programValue;
                }
            }
            function _setStoreValue(v) {
                store = store;
                switch (as) {
                    case 0:
                        store.value = v;
                        break;
                    case 1:
                        store.userValue = v;
                        break;
                    case 2:
                        store.programValue = v;
                        break;
                }
            }
            if (value === undefined) {
                // Special case of deletion. If the property exists, we set its value to undefined, and if that property is
                // defined to be inheritable, we set this.hasChangedInheritableProperties to 2. Note that an inherited property
                // cannot be deleted, as that would be inconsistent; it can only be overridden.
                // trivial case, don't do anything
                if (!store || getStoreValue() === undefined)
                    return value;
                if (store.inherit === 1) {
                    // If the store has an inheritance value of 1, we don't do anything
                    return value;
                }
                else if (store.inherit === 2) {
                    // If the property has inheritance 2, we keep it as undefined and notify that the signature of inheritable properties has
                    // changed.
                    _setStoreValue(undefined);
                    // If setting the real value, need to change the version
                    if (as === 0) {
                        store.version = getVersionID();
                        if (markChanged)
                            this.markHasChangedInheritanceSignature();
                    }
                }
                else {
                    // Set its value to undefined
                    _setStoreValue(undefined);
                }
                if (markChanged) {
                    // Mark which bit has changed
                    store.changed |= 1 << as;
                    this.hasChangedProperties |= 1 << as;
                }
                return undefined;
            }
            // Otherwise, we need to get a property store
            if (!store)
                store = this._createPropertyStore(propName);
            // We reject assignments to an inherited property. This can be overridden by setting the property's inheritance
            // status.
            if (store.inherit === 1)
                return value;
            if (equalityCheck !== 0) {
                let storeValue = getStoreValue();
                // Perform various equality checks
                if (equalityCheck === 1 && storeValue === value)
                    return value;
                else if (equalityCheck > 1 && deepEquals(storeValue, value, equalityCheck === 3))
                    return value;
            }
            // Set the value and changed values
            _setStoreValue(value);
            if (markChanged) {
                store.changed |= 1 << as;
                this.hasChangedProperties |= 1 << as;
                // For values to be inherited, store the version of this value. Only for inherit: 2 properties
                if (store.inherit === 2 && as === 0) {
                    store.version = getVersionID();
                    this.markHasChangedInheritableProperties();
                }
            }
            return value;
        }
        markHasChangedProperties() {
            this.hasChangedProperties |= 0b1;
        }
        markHasChangedInheritableProperties() {
            let c = this.hasChangedInheritableProperties;
            this.hasChangedInheritableProperties = (c > 1) ? c : 1;
        }
        markHasChangedInheritanceSignature() {
            this.hasChangedInheritableProperties = 2;
        }
        configureProperty(propName, opts = {}) {
            if (opts.inherit !== undefined) {
                this.setPropertyInheritance(propName, opts.inherit);
            }
        }
        configureProperties(propNames, opts = {}) {
            for (const propName of propNames)
                this.configureProperty(propName, opts);
        }
        /**
         * Set a property's inheritance to 2 (if inherit is true) or 0
         * @param propName {string}
         * @param inherit {boolean}
         * @return {Props}
         */
        setPropertyInheritance(propName, inherit = false) {
            // Force a property store to be created with undefined value
            const store = this._createPropertyStore(propName);
            let currentInheritance = !!store.inherit;
            if (currentInheritance === inherit)
                return this;
            if (inherit) {
                store.version = getVersionID();
                store.inherit = 2;
            }
            else {
                delete store.version;
                delete store.inherit;
            }
            if (store.value !== undefined)
                this.hasChangedInheritableProperties = 2;
            return this;
        }
        /**
         * Get the value of a property.
         * @param propName {string}
         * @param as {number} 0 if getting the real value, 1 if getting the user value, 2 if getting the program value
         * @returns {*}
         */
        get(propName, as = 0) {
            let store = this._getPropertyStore(propName);
            if (!store)
                return undefined;
            switch (as) {
                case 0:
                    return store.value;
                case 1:
                    return store.userValue;
                case 2:
                    return store.programValue;
            }
        }
        getUserValue(propName) {
            return this.get(propName, 1);
        }
        getProgramValue(propName) {
            return this.get(propName, 2);
        }
        /**
         * Get the values of a list of properties.
         * @param propNameList
         */
        getProperties(propNameList) {
            return propNameList.map(propName => this.get(propName));
        }
        /**
         * Mark all properties as locally updated (changed = false).
         */
        markAllUpdated(bitmask = 0b111) {
            bitmask = ~bitmask;
            this.hasChangedProperties &= bitmask;
            this._forEachStore(store => {
                store.changed &= bitmask;
            });
        }
        /**
         * Mark a specific property as locally updated (changed = false).
         * @param propName
         */
        markPropertyUpdated(propName) {
            const store = this._getPropertyStore(propName);
            if (store)
                store.changed = 0;
        }
        /**
         * Mark a given property (if it exists) as changed.
         * @param propName {string}
         */
        markChanged(propName) {
            let store = this._getPropertyStore(propName);
            if (!store)
                return;
            store.changed |= 0b1;
            this.hasChangedProperties |= 0b1;
            // If the store is inheritable, we need to generate a version ID
            if (store.inherit) {
                store.version = getVersionID();
                this.markHasChangedInheritableProperties();
            }
        }
        /**
         * Mark that no more inheritance is necessary. This function should only be called by the scene
         */
        _markGlobalUpdateComplete() {
            if (this.hasChangedProperties)
                this.markAllUpdated();
            this.hasChangedInheritableProperties = 0;
        }
        stringify() {
            const obj = {};
            for (const [propName, propStore] of this.store) {
                obj[propName] = propStore;
            }
            console.log(JSON.stringify(obj, null, 4));
        }
    }
    const proxyHandlers = {
        get: (target, propName) => {
            return target.get(propName);
        },
        set: (target, propName, value) => {
            target.set(propName, value);
            return true;
        }
    };

    /**
     * The element class.
     */
    class Element extends Eventful {
        constructor(opts = {}) {
            var _a;
            super(); // Eventful
            this.id = (_a = opts.id) !== null && _a !== void 0 ? _a : getStringID();
            if (typeof this.id !== 'string' || this.id.length === 0)
                throw new TypeError('The element id must be a non-empty string');
            this.parent = null;
            this.scene = null;
            this.props = new Props();
            this.updateStage = -1;
            this.internal = {
                version: getVersionID(),
                order: 0,
                needsOrdering: false
            };
            // Call the element-defined constructor
            this.init(opts);
        }
        /**
         * Set the order of this element, which determines which position the element will be in the group it is a part of
         * @param o Order
         */
        setOrder(o) {
            if (!Number.isFinite(o) || typeof o !== "number")
                throw new RangeError("Order must be a finite number");
            this.internal.order = o;
            if (this.parent)
                this.parent._markNeedsOrdering();
        }
        /**
         * Get the order of this element, which determines which position the element will be in the group it is a part of
         */
        getOrder() {
            return this.internal.order;
        }
        /**
         * Derived class–implemented function that is called when the element needs to be updated (usually to change how
         * it's displayed)
         */
        _update() { }
        /**
         * Add a given element as a child to this element. Fails on elements that are not groups.
         * @param e Element to add
         */
        add(e) {
            throw new Error("Element is not a group and does not support having children");
        }
        /**
         * Apply a callback function, accepting a single argument (the element)
         * @param callback Callback function
         */
        apply(callback) {
            callback(this);
        }
        /**
         * Apply two callback functions, one before the children are called, and one after
         * @param callback1 Callback function to be called before
         * @param callback2 Callback function to be called after
         */
        applyTwice(callback1, callback2) {
            // TODO
        }
        /**
         * Inherit properties from the parent. If the updateStage is -1, then it indicates the child has not inherited any
         * properties yet at all, so we need to check them all.
         */
        _defaultInheritProps() {
            if (this.parent)
                this.props.inheritPropertiesFrom(this.parent.props, this.updateStage === -1);
        }
        /**
         * Default rendering info information, which just pulls from internal.renderInfo
         */
        getRenderingInfo() {
            if (this.internal.renderInfo)
                return this.internal.renderInfo;
            return null;
        }
        /**
         * Check whether a given element is a child of this element
         * @param child
         * @param recursive Whether to search recursively, or just look for immediate children
         */
        isChild(child, recursive = true) {
            return false;
        }
        /**
         * Whether this element is a top-level scene
         */
        isScene() {
            return false;
        }
        init(params) { }
        _defaultComputeProps() {
        }
        /**
         * Recursively set the scene which this element belongs to
         * @param scene The scene
         */
        _setScene(scene) {
            this.scene = scene;
        }
        /**
         * Stringify the props contents of this element
         */
        stringify() {
            this.props.stringify();
        }
        /**
         * Sort children by ordering order. If force is true, sorts whether it's marked as needing ordering or not
         */
        sortChildren(force = false) {
        }
        update() {
            // If some properties have changed, set the update stage accordingly. We use .min in case the update stage is -1
            if (this.props.hasChangedProperties)
                this.updateStage = Math.min(this.updateStage, 0);
            if (this.updateStage === 100)
                return;
            this._update();
            this.updateStage = 100;
        }
        getChildren() {
            // @ts-ignore
            return this.isGroup() ? this.children : [];
        }
        isGroup() {
            return false;
        }
    }

    class Group extends Element {
        constructor(params) {
            super(params);
            this.children = [];
        }
        _update() {
            this._defaultInheritProps();
        }
        /**
         * Add an element to this group.
         * @param elem Element to add
         * @returns This group, for chaining
         */
        add(elem) {
            if (elem.isScene())
                throw new Error('Scene cannot be a child');
            if (elem.parent)
                throw new Error('Element to be added already has a parent');
            if (!(elem instanceof Element))
                throw new TypeError('Element not element');
            if (elem === this)
                throw new Error("Can't add self");
            if (elem.isChild(this))
                throw new Error("Can't make cycle");
            this.children.push(elem);
            elem.parent = this;
            elem._setScene(this.scene);
            elem.updateStage = -1;
            this._markNeedsOrdering();
            return this;
        }
        /**
         * Run callback(element) on this element and all the element's children
         * @param callback {Function}
         */
        apply(callback) {
            callback(this);
            this.children.forEach(child => child.apply(callback));
        }
        /**
         * If some inheritable properties have changed since the last global update completion, set all the children's update
         * stages to 0. May change how this works later
         */
        informChildrenOfInheritance() {
            if (this.props.hasChangedInheritableProperties && this.children) {
                this.children.forEach(child => {
                    let st = child.updateStage;
                    child.updateStage = (st < 0) ? 0 : st;
                });
            }
        }
        isChild(elem, recursive = true) {
            for (const child of this.children) {
                if (child === elem)
                    return true;
                if (recursive && child.isChild(elem, true))
                    return true;
            }
            return false;
        }
        isGroup() {
            return true;
        }
        _markNeedsOrdering() {
            this.internal.needsOrdering = true;
        }
        sortChildren(force = false) {
            if (force || this.internal.needsOrdering) {
                let children = this.children;
                if (children.length <= 1)
                    return;
                let cl = children.length;
                let orders = [];
                let indices = [];
                for (let i = 0; i < cl; ++i) {
                    orders.push(children[i].getOrder());
                    indices.push(i);
                }
                // Doing it this way reduces the number of accesses to children
                indices.sort((i1, i2) => {
                    let o1 = orders[i1], o2 = orders[i2];
                    return (o1 < o2) ? -1 : ((o1 > o2) ? 1 : 0);
                });
                // Might be a little slow... TODO perf testing
                let oldC = children.slice();
                // Swap according to new indices, in place
                for (let i = 0; i < cl; ++i) {
                    children[i] = oldC[indices[i]];
                }
            }
        }
        /**
         * Remove a direct element from this group. Fails silently if the passed element is not a child.
         * @param elem Element to remove
         * @returns This group, for chaining
         */
        remove(elem) {
            const index = this.children.indexOf(elem);
            if (index !== -1) {
                this.children.splice(index, 1);
                elem.parent = null;
                elem._setScene(null);
                elem.updateStage = -1;
            }
            this._markNeedsOrdering();
            return this;
        }
        _setScene(scene) {
            this.scene = scene;
            for (let i = 0; i < this.children.length; i++) {
                this.children[i]._setScene(scene);
            }
        }
        triggerEvent(eventName, data) {
            // Children are triggered first
            for (const child of this.children) {
                if (child.triggerEvent(eventName, data))
                    return true;
            }
            return super.triggerEvent(eventName, data);
        }
        update() {
            super.update();
            this.informChildrenOfInheritance();
        }
    }

    /**
     * Passed to children as the parameter "sceneDimensions"
     */
    class SceneDimensions {
        constructor(width, height, dpr) {
            this.width = width;
            this.height = height;
            this.dpr = dpr;
            // The size of the canvas in true device pixels, rather than CSS pixels
            this.canvasWidth = this.dpr * this.width;
            this.canvasHeight = this.dpr * this.height;
        }
        /**
         * Get the bounding box of the entire scene
         * @returns
         */
        getBoundingBox() {
            return new BoundingBox(0, 0, this.width, this.height);
        }
        clone() {
            return new SceneDimensions(this.width, this.height, this.dpr);
        }
    }

    const MIN_SIZE = 100, MAX_SIZE = 16384;
    function checkDimInRange(d) {
        if (typeof d !== "number" || Number.isNaN(d) || d < MIN_SIZE || d > MAX_SIZE) {
            throw new RangeError(`Dimension ${d} is out of range [${MIN_SIZE}, ${MAX_SIZE}]`);
        }
    }
    /**
     * Top level element in a Grapheme context. The scene has a width, height, and device pixel ratio as its defining
     * geometric patterns, and potentially other properties -- interactivity information, for example. Uniquely, every
     * element knows its scene directly as its .scene property.
     */
    class Scene extends Group {
        init(params) {
            this.scene = this;
            this.props.set('sceneDims', new SceneDimensions(640, 480, 1));
            this.props.setPropertyInheritance('sceneDims', true);
        }
        /**
         * Get the scene's dimensions
         */
        getDims() {
            return this._getDims().clone();
        }
        _getDims() {
            return this.props.get('sceneDims');
        }
        _setDims(dims) {
            this.props.set("sceneDims", dims, 0, 2);
        }
        setWidth(w) {
            checkDimInRange(w);
            let d = this.getDims();
            d.width = w;
            this._setDims(d);
            return this;
        }
        setHeight(h) {
            checkDimInRange(h);
            let d = this.getDims();
            d.width = h;
            this._setDims(d);
            return this;
        }
        setDPR(dpr) {
            checkDimInRange(dpr);
            let d = this.getDims();
            d.width = dpr;
            this._setDims(d);
            return this;
        }
        /**
         * Compute the internal property "sceneDimensions"
         */
        calculateSceneDimensions() {
            const { props } = this;
            if (props.haveChanged(['width', 'height', 'dpr'])) {
                const { width, height, dpr } = props.proxy;
                const sceneDimensions = new SceneDimensions(width, height, dpr);
                // Equality check of 2 for deep comparison, in case width, height, dpr have not actually changed
                props.set('sceneDims', sceneDimensions, 0 /* real */, 2 /* equality check */);
            }
        }
        updateProps() {
            this._defaultComputeProps();
            this.calculateSceneDimensions();
        }
        /**
         * Only scenes (and derived scenes) return true
         * @returns {boolean}
         */
        isScene() {
            return true;
        }
        _update() {
            this.updateProps();
            this.internal.renderInfo = {
                contexts: [{
                        insnType: 'scene',
                        dims: this.props.get('sceneDims'),
                        backgroundColor: this.props.get('backgroundColor')
                    }]
            };
        }
        /**
         * This function updates all the elements and is the only one with the authority to mark all properties, including
         * inheritable properties, as unchanged.
         */
        updateAll() {
            this.apply(child => {
                child.update();
            });
            // Mark the update as completed (WIP)
            this.apply(child => child.props._markGlobalUpdateComplete());
        }
    }

    function toDir(obj) {
        if (obj instanceof Vec2) {
            return obj;
        }
        else if (typeof obj === "string") {
            switch (obj) {
                case "N": return new Vec2(0, -1);
                case "S": return new Vec2(0, 1);
                case "W": return new Vec2(1, 0);
                case "E": return new Vec2(-1, 0);
                case "NE": return new Vec2(-1, -1);
                case "NW": return new Vec2(1, -1);
                case "SE": return new Vec2(-1, 1);
                case "SW": return new Vec2(1, 1);
                case "C": return new Vec2(0, 0);
            }
        }
        else if (typeof obj === "undefined") {
            return new Vec2(0, 0);
        }
        else {
            throw new TypeError("Invalid direction");
        }
    }
    function calculateRectShift(rect, dir, spacing) {
        dir = toDir(dir);
        let shiftX = dir.x * rect.w / 2, shiftY = dir.y * rect.h / 2;
        let shiftLen = Math.hypot(shiftX, shiftY);
        let scaleSpacing = (shiftLen === 0) ? 0 : (shiftLen + spacing) / shiftLen;
        shiftX *= scaleSpacing;
        shiftY *= scaleSpacing;
        shiftX += -rect.w / 2;
        shiftY += -rect.h / 2;
        return new BoundingBox(rect.x + shiftX, rect.y + shiftY, rect.w, rect.h);
    }

    /**
     * A scene endowed with an actual DOM element.
     */
    class InteractiveScene extends Scene {
        init(params) {
            var _a;
            super.init(params);
            this.domElement = document.createElement("div");
            this.domElement.style.position = "relative"; // so that absolute html children are positioned relative to the div
            this.domCanvas = document.createElement('canvas');
            this.domCanvas.id = this.id;
            this.domElement.appendChild(this.domCanvas);
            this.bitmapRenderer = (_a = this.domCanvas.getContext('bitmaprenderer')) !== null && _a !== void 0 ? _a : throwNoBitmapRenderer();
        }
        _disableInteractivityListeners() {
            let internal = this.internal;
            let interactivityListeners = internal.interactivityListeners;
            if (!interactivityListeners)
                return;
            for (let listenerType in interactivityListeners) {
                let listener = interactivityListeners[listenerType];
                this.domElement.removeEventListener(listenerType, listener);
            }
            internal.interactivityListeners = null;
        }
        _enableInteractivityListeners() {
            this._disableInteractivityListeners();
            let listeners = (this.internal.interactivityListeners = {});
            // Convert mouse event coords (which are relative to the top left corner of the page) to canvas coords
            const getSceneCoords = evt => {
                let rect = this.domElement.getBoundingClientRect();
                return new Vec2(evt.clientX - rect.x, evt.clientY - rect.y);
            };
            ['mousedown', 'mousemove', 'mouseup', 'wheel'].forEach(eventName => {
                let listener;
                if (eventName === 'wheel') {
                    listener = evt => {
                        this.triggerEvent(eventName, {
                            pos: getSceneCoords(evt),
                            deltaY: evt.deltaY
                        });
                        evt.preventDefault();
                    };
                }
                else {
                    listener = evt => {
                        this.triggerEvent(eventName, { pos: getSceneCoords(evt) });
                        evt.preventDefault();
                    };
                }
                let elem = eventName === "mouseup" ? document : this.domElement;
                elem.addEventListener(eventName, (listeners[eventName] = listener));
            });
        }
        setInteractivity(enable) {
            this.props.set("interactivity", enable, 0, 1);
            this._updateInteractivity();
        }
        _updateInteractivity() {
            let enable = this.props.get("interactivity");
            let internal = this.internal;
            if (!!internal.interactivityListeners !== enable) {
                enable
                    ? this._enableInteractivityListeners()
                    : this._disableInteractivityListeners();
            }
        }
        _update() {
            super._update();
            this._updateInteractivity();
            this.resizeCanvas();
        }
        resizeCanvas() {
            if (this.props.hasChanged("sceneDims")) {
                let sceneDims = this._getDims();
                let c = this.domCanvas;
                c.width = sceneDims.canvasWidth;
                c.height = sceneDims.canvasHeight;
                c.style.width = sceneDims.width + 'px';
                c.style.height = sceneDims.height + 'px';
            }
        }
        addHTMLElement(element) {
            let domElement = this.domElement;
            domElement.appendChild(element);
        }
        setHTMLElements(instructions) {
            let internal = this.internal, htmlElements;
            let that = this; // huzzah
            if (!internal.htmlElements)
                internal.htmlElements = [];
            htmlElements = internal.htmlElements;
            htmlElements.forEach(elem => elem.claimed = false);
            function addElementToDOM(html) {
                let div = document.createElement("div");
                div.innerHTML = html;
                div.style.position = "absolute";
                div.style.left = div.style.top = '0';
                div.style.visibility = "none";
                that.domElement.appendChild(div);
                let rect = div.getBoundingClientRect();
                return { div, rect };
            }
            function addElement(html, pos, dir, spacing, transform) {
                let { div, rect } = addElementToDOM(html);
                let shiftedRect = calculateRectShift(new BoundingBox(pos.x, pos.y, rect.width, rect.height), dir, spacing);
                div.style.left = shiftedRect.x + 'px';
                div.style.top = shiftedRect.y + 'px';
                div.style.transform = transform;
                return {
                    type: "html", pos: new Vec2(shiftedRect.x, shiftedRect.y), domElement: div,
                    w: rect.width, h: rect.height, claimed: true, content: ""
                };
            }
            main: for (const instruction of instructions) {
                if (instruction.type === "latex") {
                    let { pos, dir, spacing, scale } = instruction;
                    for (const elem of htmlElements) {
                        if (elem.claimed || elem.type !== "latex" || elem.content !== instruction.content)
                            continue;
                        // then the element's latex content is the same, so we calculate the new position. Note we reuse the old
                        // width/height values so that getBoundingClientRect() is only called once
                        let shiftedRect = calculateRectShift(new BoundingBox(pos.x, pos.y, elem.w, elem.h), dir, spacing);
                        pos = shiftedRect.tl();
                        if (elem.pos.x !== pos.x || elem.pos.y !== pos.y) { // need to move the element
                            elem.domElement.style.left = shiftedRect.x + 'px';
                            elem.domElement.style.top = shiftedRect.y + 'px';
                            elem.pos = pos;
                        }
                        elem.claimed = true;
                        continue main;
                    }
                    // No latex element exists that's unclaimed and has the same content, so we create one
                    let elem = addElement(instruction.html, pos, dir, spacing, `matrix(${scale}, 0, 0, ${scale}, 0, 0)`);
                    elem.type = "latex";
                    elem.content = instruction.content;
                    htmlElements.push(elem);
                }
            }
            // Destroy unclaimed html elements
            this.internal.htmlElements = htmlElements.filter(elem => {
                let claimed = elem.claimed;
                if (!claimed) {
                    this.domElement.removeChild(elem.domElement);
                }
                return claimed;
            });
        }
        destroyHTMLElements() {
            let children = Array.from(this.domElement.children);
            for (const child of children) {
                if (child.id !== this.id) {
                    child.style.visibility = "none";
                    this.domElement.removeChild(child);
                }
            }
        }
    }
    function throwNoBitmapRenderer() {
        throw new Error("Browser does not support bitmap renderer");
    }

    const wasmSupported = (function () {
        // Attempt to compile a module that uses v128
        let supported = true;
        try {
            new WebAssembly.Module(toBuffer("\x00asm\x01\x00\x00\x00\x01\x05\x01`\x01{\x00\x03\x02\x01\x00\n\x04\x01\x02\x00\v")); // simd_test.wat
        }
        catch (e) {
            supported = false;
        }
        return supported;
    })();
    function toBuffer(bin) {
        let buffer = new Uint8Array(bin.length);
        for (let i = 0; i < bin.length; ++i) {
            buffer[i] = bin.charCodeAt(i);
        }
        return buffer;
    }
    const WASM = (function () {
        if (wasmSupported)
            try {
                // base-64 encoded. DO NOT CHANGE THE FOLLOWING LINE MANUALLY
                const WASM_CONTENTS = /* $WASM_CONTENTS */ "AGFzbQEAAAABEgRgAX8AYAF9AGABfABgAn9/AAIrAwdjb25zb2xlA2xvZwAAB2NvbnNvbGUDbG9nAAEHY29uc29sZQNsb2cAAgMCAQMFBAEAkE4HIgIVYm91bmRpbmdfYm94X2ZsYXRfZjMyAAMGbWVtb3J5AgAKkwEBkAECAX8De/0MAACAfwAAgH8AAIB/AACAfyED/QwAAID/AACA/wAAgP8AAID/IQQgACABaiECAkADQCADIAD9AAQAIgX96gEhAyAEIAX96wEhBCADIABBEGr9AAQAIgX96gEhAyAEIAX96wEhBCAAQSBqIgAgAk4EQAwCBQwBCwsLQQAgA/0LBABBECAE/QsEAAs=";
                function log(o) {
                    console.log(o);
                }
                const module = new WebAssembly.Module(toBuffer(atob(WASM_CONTENTS)));
                const instance = new WebAssembly.Instance(module, {
                    js: {
                        mem: new WebAssembly.Memory({ initial: 0, maximum: 10000 /* 64 kib pages */ })
                    }, console: { log }
                });
                let memory = instance.exports.memory;
                const HEAP_F32 = new Float32Array(memory.buffer);
                const HEAP_F64 = new Float64Array(memory.buffer);
                const HEAP_I32 = new Int32Array(memory.buffer);
                const HEAP_U32 = new Uint32Array(memory.buffer);
                const bounding_box_flat_f32 = instance.exports.bounding_box_flat_f32;
                /**
                 * Size in bytes; returns location of a pointer
                 * @param size
                 */
                function malloc(size) {
                    size |= 0;
                }
                function boundingBoxFlatF32(arr) {
                    // Copy into buffer. First 8 entries (32 bytes) are the found minima and maxima
                    HEAP_F32.set(arr, 8);
                    // Fill extra space with NaNs because the internal implementation doesn't take care of the edges
                    let len = arr.length | 0, endFill = (15 + len) & (~0b11);
                    for (let i = 8 + len; i < endFill; ++i) {
                        HEAP_F32[i] = NaN;
                    }
                    bounding_box_flat_f32(32, arr.length << 2);
                    let xmin1 = HEAP_F32[0];
                    let ymin1 = HEAP_F32[1];
                    let xmin2 = HEAP_F32[2];
                    let ymin2 = HEAP_F32[3];
                    let xmax1 = HEAP_F32[4];
                    let ymax1 = HEAP_F32[5];
                    let xmax2 = HEAP_F32[6];
                    let ymax2 = HEAP_F32[7];
                    let xmin = xmin1 < xmin2 ? xmin1 : xmin2;
                    let ymin = ymin1 < ymin2 ? ymin1 : ymin2;
                    let xmax = xmax1 > xmax2 ? xmax1 : xmax2;
                    let ymax = ymax1 > ymax2 ? ymax1 : ymax2;
                    if (xmin === Infinity || ymin === Infinity) { // no valid entries found
                        return null;
                    }
                    return new BoundingBox(xmin, ymin, roundUp(xmax - xmin), roundUp(ymax - ymin));
                }
                return {
                    HEAP_F32,
                    HEAP_F64,
                    HEAP_I32,
                    HEAP_U32,
                    instance,
                    exports: instance.exports,
                    boundingBoxFlatF32,
                    supported: true
                };
            }
            catch (e) {
            }
        console.warn("WebAssembly not supported; default JS implementations will be used");
        return {
            supported: false
        };
    })();

    var _a;
    // Credit to cortijon on StackOverflow (comment on https://stackoverflow.com/a/1968345/13458117)
    function getLineIntersection(p0_x, p0_y, p1_x, p1_y, p2_x, p2_y, p3_x, p3_y) {
        let s1_x, s1_y, s2_x, s2_y;
        s1_x = p1_x - p0_x;
        s1_y = p1_y - p0_y;
        s2_x = p3_x - p2_x;
        s2_y = p3_y - p2_y;
        const s = (-s1_y * (p0_x - p2_x) + s1_x * (p0_y - p2_y)) /
            (-s2_x * s1_y + s1_x * s2_y);
        const t = (s2_x * (p0_y - p2_y) - s2_y * (p0_x - p2_x)) / (-s2_x * s1_y + s1_x * s2_y);
        if (s >= 0 && s <= 1 && t >= 0 && t <= 1) {
            // Collision detected
            const intX = p0_x + t * s1_x;
            const intY = p0_y + t * s1_y;
            return [intX, intY];
        }
        return null;
    }
    /**
     * Returns whether the point p is in the triangle generated by a, b, and c. Does not specially handle NaN values and the
     * like.
     * @param px x-coordinate of point p
     * @param py y-coordinate of point p
     * @param ax x-coordinate of point a
     * @param ay y-coordinate of point a
     * @param bx x-coordinate of point b
     * @param by y-coordinate of point b
     * @param cx x-coordinate of point c
     * @param cy
     */
    function pointInTriangle(px, py, ax, ay, bx, by, cx, cy) {
        const v0x = cx - ax;
        const v0y = cy - ay;
        const v1x = bx - ax;
        const v1y = by - ay;
        const v2x = px - ax;
        const v2y = py - ay;
        const dot00 = v0x * v0x + v0y * v0y;
        const dot01 = v0x * v1x + v0y * v1y;
        const dot02 = v0x * v2x + v0y * v2y;
        const dot11 = v1x * v1x + v1y * v1y;
        const dot12 = v1x * v2x + v1y * v2y;
        const invDenom = 1 / (dot00 * dot11 - dot01 * dot01);
        const u = (dot11 * dot02 - dot01 * dot12) * invDenom;
        const v = (dot00 * dot12 - dot01 * dot02) * invDenom;
        return u >= 0 && v >= 0 && u + v < 1;
    }
    function lineSegmentIntersectsBox(x1, y1, x2, y2, box_x1, box_y1, box_x2, box_y2) {
        // Return the component of the line segment that resides inside a box with boundaries x in (box_x1 .. box_x2), y in
        // (box_y1 .. box_y2), which may potentially be the entire line segment.
        let pt1InBox = box_x1 <= x1 && x1 <= box_x2 && box_y1 <= y1 && y1 <= box_y2;
        let pt2InBox = box_x1 <= x2 && x2 <= box_x2 && box_y1 <= y2 && y2 <= box_y2;
        if (pt1InBox && pt2InBox) {
            // The line segment is entirely in the box
            return [x1, y1, x2, y2];
        }
        // Infinities cause weird problems with getLineIntersection, so we just approximate them lol
        if (x1 === Infinity)
            x1 = 1e6;
        else if (x1 === -Infinity)
            x1 = -1e6;
        if (x2 === Infinity)
            x2 = 1e6;
        else if (x2 === -Infinity)
            x2 = -1e6;
        if (y1 === Infinity)
            y1 = 1e6;
        else if (y1 === -Infinity)
            y1 = -1e6;
        if (y2 === Infinity)
            y2 = 1e6;
        else if (y2 === -Infinity)
            y2 = -1e6;
        let int1 = getLineIntersection(x1, y1, x2, y2, box_x1, box_y1, box_x2, box_y1);
        let int2 = getLineIntersection(x1, y1, x2, y2, box_x2, box_y1, box_x2, box_y2);
        let int3 = getLineIntersection(x1, y1, x2, y2, box_x2, box_y2, box_x1, box_y2);
        let int4 = getLineIntersection(x1, y1, x2, y2, box_x1, box_y2, box_x1, box_y1);
        if (!(int1 || int2 || int3 || int4) && !pt1InBox && !pt2InBox) {
            // If there are no intersections and the points are outside the box, that means none of the segment is inside the
            // box, so we can return null
            return null;
        }
        let intersections = [int1, int2, int3, int4];
        if (!pt1InBox && !pt2InBox) {
            // Both points are outside of the box, but the segment intersects the box. I'm frustrated! We must RESTRICT by finding the pair of intersections with
            // maximal separation. This deals with annoying corner cases. Thankfully this code doesn't need to be too efficient
            // since this is a rare case.
            let maximalSeparationSquared = -1;
            let n_x1, n_y1, n_x2, n_y2;
            for (let i = 0; i < 3; ++i) {
                let i1 = intersections[i];
                if (i1) {
                    for (let j = i + 1; j < 4; ++j) {
                        let i2 = intersections[j];
                        if (i2) {
                            let dist = (i2[0] - i1[0]) ** 2 + (i2[1] - i1[1]) ** 2;
                            if (dist > maximalSeparationSquared) {
                                maximalSeparationSquared = dist;
                                n_x1 = i1[0];
                                n_y1 = i1[1];
                                n_x2 = i2[0];
                                n_y2 = i2[1];
                            }
                        }
                    }
                }
            }
            // Swap the order if necessary. We need the result of this calculation to be in the same order as the points
            // that went in, since this will be used in the dashed line logic.
            if (n_x1 < n_x2 === x1 > x2 || n_y1 < n_y2 === y1 > y2) {
                let tmp = n_x1;
                n_x1 = n_x2;
                n_x2 = tmp;
                tmp = n_y1;
                n_y1 = n_y2;
                n_y2 = tmp;
            }
            return [n_x1, n_y1, n_x2, n_y2];
        }
        if (pt1InBox) {
            for (let i = 0; i < 4; ++i) {
                let intersection = intersections[i];
                if (intersection)
                    return [x1, y1, intersection[0], intersection[1]];
            }
        }
        else if (pt2InBox) {
            for (let i = 0; i < 4; ++i) {
                let intersection = intersections[i];
                if (intersection)
                    return [intersection[0], intersection[1], x2, y2];
            }
        }
        return [x1, y1, x2, y2];
    }
    function generateCircleTriangleStrip(radius, x = 0, y = 0, samples = 8) {
        const points = [];
        for (let i = 0; i <= samples; ++i) {
            const angle = (i / samples) * 2 * Math.PI;
            const xc = x + radius * Math.cos(angle), yc = y + radius * Math.sin(angle);
            if (i % 2 === 0) {
                points.push(xc, yc);
                points.push(x, y);
            }
            else {
                points.push(xc, yc);
            }
        }
        points.push(NaN, NaN);
        return new Float32Array(points);
    }
    function generateRectangleTriangleStrip(rect) {
        const { x, y, w, h } = rect;
        const points = [x, y, x + w, y, x, y + h, x + w, y + h];
        return new Float32Array(points);
    }
    /**
     * Given a rectangle, return a flat list of points enclosing a cycle around the rectangle.
     * @param rect {BoundingBox}
     * @returns {Float32Array}
     */
    function generateRectangleCycle(rect) {
        const { x, y, w, h } = rect;
        const points = [x, y, x + w, y, x + w, y + h, x, y + h, x, y];
        return new Float32Array(points);
    }
    function generateRectangleDebug(rect) {
        const { x, y, w, h } = rect;
        const points = [x, y, x + w, y, x + w, y + h, x, y + h, x, y, x + w, y + w];
        return new Float32Array(points);
    }
    // Given a Float32Array of appropriate size, repeatedly add given triangle strips
    function combineTriangleStrips(verticesBuff) {
        let index = 0;
        return arr => {
            if (arr.length === 0)
                return;
            // Repeat previous vertex
            if (index > 0) {
                verticesBuff[index] = verticesBuff[index - 2];
                verticesBuff[index + 1] = verticesBuff[index - 1];
                verticesBuff[index + 2] = arr[0];
                verticesBuff[index + 3] = arr[1];
                index += 4;
            }
            verticesBuff.set(arr, index);
            index += arr.length;
        };
    }
    function combineColoredTriangleStrips(verticesBuff, colorBuff) {
        let index = 0;
        return (arr, { r = 0, g = 0, b = 0, a = 0 }) => {
            if (arr.length === 0)
                return;
            // Repeat previous vertex
            if (index > 0) {
                verticesBuff[index] = verticesBuff[index - 2];
                verticesBuff[index + 1] = verticesBuff[index - 1];
                verticesBuff[index + 2] = arr[0];
                verticesBuff[index + 3] = arr[1];
                index += 4;
            }
            verticesBuff.set(arr, index);
            fillRepeating(colorBuff, [r / 255, g / 255, b / 255, a / 255], index * 2, 2 * (index + arr.length));
            index += arr.length;
        };
    }
    /**
     * Fill the TypedArray arr with a given pattern throughout [startIndex, endIndex). Works if either is out of bounds.
     * Worst code ever. Uses copyWithin to try make the operation FAST for large arrays (not optimized for small ones). On
     * a 50000-element array in my chrome, it provides a 16x speedup.
     * @param arr Array to fill
     * @param pattern Pattern to fill with
     * @param startIndex Index of the first instance of the pattern
     * @param endIndex Index immediately after the last instance of the pattern
     * @param patternStride Offset to begin copying the pattern
     * @returns The original array
     */
    function fillRepeating(arr, pattern, startIndex = 0, endIndex = arr.length, patternStride = 0) {
        if (endIndex <= startIndex)
            return arr;
        let patternLen = pattern.length, arrLen = arr.length;
        if (patternLen === 0)
            return arr;
        endIndex = Math.min(endIndex, arrLen);
        if (endIndex <= 0 || startIndex >= arrLen)
            return arr;
        if (startIndex < 0) {
            patternStride -= startIndex;
            startIndex = 0;
        }
        if (patternStride !== 0)
            patternStride = mod(patternStride, patternLen);
        let filledEndIndex = Math.min(endIndex, startIndex + patternLen);
        let i, j;
        for (i = startIndex, j = patternStride; i < filledEndIndex && j < patternLen; ++i, ++j) {
            arr[i] = pattern[j];
        }
        // For nonzero strides
        for (j = 0; i < filledEndIndex; ++i, ++j) {
            arr[i] = pattern[j];
        }
        if (filledEndIndex === endIndex)
            return arr;
        // We now need to iteratively copy [startIndex, startIndex + filledLen) to [startIndex + filledLen, endIndex) and
        // double filledLen accordingly. memcpy, take the wheel!
        let filledLen = patternLen;
        while (true) {
            let copyLen = Math.min(filledLen, endIndex - filledEndIndex);
            arr.copyWithin(filledEndIndex, startIndex, startIndex + copyLen);
            filledEndIndex += copyLen;
            filledLen += copyLen;
            // Should never be greater, but whatever
            if (filledEndIndex >= endIndex)
                return arr;
        }
    }
    function _flattenVec2ArrayInternal(arr) {
        var _a;
        const out = [];
        for (let i = 0; i < arr.length; ++i) {
            let item = arr[i];
            if (item === null || item === undefined) {
                out.push(NaN, NaN);
            }
            else if (item.x !== undefined && item.y !== undefined) {
                out.push(item.x, item.y);
            }
            else if (item[0] !== undefined) {
                out.push(+item[0], (_a = item[1]) !== null && _a !== void 0 ? _a : 0);
            }
            else {
                if (typeof item === 'number')
                    out.push(item);
                else
                    throw new TypeError(`Error when converting array to flattened Vec2 array: Unknown item ${item} at index ${i} in given array`);
            }
        }
        return out;
    }
    // Given some arbitrary array of Vec2s, turn it into the regularized format [x1, y1, x2, y2, ..., xn, yn]. The end of
    // one polyline and the start of another is done by one pair of numbers being NaN, NaN.
    function flattenVec2Array(arr) {
        if (isTypedArray(arr))
            return arr;
        for (let i = 0; i < arr.length; ++i) {
            if (typeof arr[i] !== 'number')
                return _flattenVec2ArrayInternal(arr);
        }
        return arr;
    }
    function fastAtan2$1(y, x) {
        let abs_x = Math.abs(x);
        let abs_y = Math.abs(y);
        let a = abs_x < abs_y ? abs_x / abs_y : abs_y / abs_x;
        // atan(x) is about x - x^3 / 3 + x^5 / 5. We also note that atan(1/x) = pi/2 - atan(x) for x > 0, etc.
        let s = a * a;
        let r = ((-0.0464964749 * s + 0.15931422) * s - 0.327622764) * s * a + a;
        if (abs_y > abs_x)
            r = 1.57079637 - r;
        if (x < 0.0)
            r = 3.14159265 - r;
        if (y < 0.0)
            r = -r;
        return r;
    }
    /**
     * Compute Math.hypot(x, y), but since all the values of x and y we're using here are not extreme, we don't have to
     * handle overflows and underflows with much accuracy at all. We can thus use the straightforward calculation.
     * Chrome: 61.9 ms/iteration for 1e7 calculations for fastHypot; 444 ms/iteration for Math.hypot
     * @param x {number}
     * @param y {number}
     * @returns {number} hypot(x, y)
     */
    function fastHypot(x, y) {
        return Math.sqrt(x * x + y * y);
    }
    function _boundingBoxFlatF32(v) {
        let minX = Infinity, minY = Infinity, maxX = -Infinity, maxY = -Infinity;
        let len = v.length;
        let i = 0;
        for (; i < len; i += 4) {
            // Unrolling the loop like this slightly improves efficiency
            let x1 = v[i];
            let y1 = v[i + 1];
            let x2 = v[i + 2];
            let y2 = v[i + 3];
            minX = minX > x1 ? x1 : minX;
            minY = minY > y1 ? y1 : minY;
            maxX = maxX < x1 ? x1 : maxX;
            maxY = maxY < y1 ? y1 : maxY;
            minX = minX > x2 ? x2 : minX;
            minY = minY > y2 ? y2 : minY;
            maxX = maxX < x2 ? x2 : maxX;
            maxY = maxY < y2 ? y2 : maxY;
        }
        if (i <= len - 2) {
            // Last two entries
            let x2 = v[i], y2 = v[i + 1];
            minX = minX > x2 ? x2 : minX;
            minY = minY > y2 ? y2 : minY;
            maxX = maxX < x2 ? x2 : maxX;
            maxY = maxY < y2 ? y2 : maxY;
        }
        return (minX === Infinity || minY === Infinity) ? null :
            new BoundingBox(minX, minY, roundUp(maxX) - minX, roundUp(maxY) - minY); // round up ensures the resultant bbox will contain it
    }
    /**
     * Compute the bounding box of a flat 2D array. Returns null if the input contains no valid points.
     */
    const computeBoundingBox = (_a = WASM.boundingBoxFlatF32) !== null && _a !== void 0 ? _a : _boundingBoxFlatF32;

    // This code is pretty old, but surprisingly effective!
    /**
     * The maximum number of vertices to be emitted by getDashedPolyline. This condition is here just to prevent dashed
     * polyline from causing a crash from OOM or just taking forever to finish.
     * @type {number}
     */
    const MAX_DASHED_POLYLINE_VERTICES = 1e7;
    /**
     * Convert a polyline into another polyline, but with dashes.
     * @param vertices {Array} The vertices of the polyline.
     * @param pen {Pen} The polyline's pen
     * @param box {BoundingBox} The plotting box, used to clip excess portions of the polyline. There could theoretically be
     * an infinite number of dashes in a long vertical asymptote, for example, but this box condition prevents that from
     * being an issue. Portions of the polyline outside the plotting box are simply returned without dashes.
     * @returns {Array}
     */
    function getDashedPolyline(vertices, pen, box) {
        if (!box)
            box = new BoundingBox(-Infinity, -Infinity, Infinity, Infinity);
        // dashPattern is the pattern of dashes, given as the length (in pixels) of consecutive dashes and gaps.
        // dashOffset is the pixel offset at which to start the dash pattern, beginning at the start of every sub polyline.
        let { dashPattern, dashOffset } = pen;
        // If the dash pattern is odd in length, concat it to itself, creating a doubled, alternating dash pattern
        if (dashPattern.length % 2 === 1)
            dashPattern = dashPattern.concat(dashPattern);
        // The length, in pixels, of the pattern
        const patternLength = dashPattern.reduce((a, b) => a + b);
        // If the pattern is invalid in some way (NaN values, negative dash lengths, total length less than 2), return the
        // polyline without dashes.
        if (patternLength < 2 ||
            dashPattern.some(dashLen => dashLen < 0) ||
            dashPattern.some(Number.isNaN))
            return vertices;
        // currentIndex is the current position in the dash pattern. currentLesserOffset is the offset within the dash or gap
        // ----    ----    ----    ----    ----    ----    ----  ... etc.
        //      ^
        // If we are there, then currentIndex is 1 and currentLesserOffset is 1.
        let currentIndex = 0, currentLesserOffset = 0;
        // Initialize the value of currentLesserOffset based on dashOffset and dashPattern
        recalculateOffset(0);
        // The returned dashed vertices
        const result = [];
        // The plotting box
        const boxX1 = box.x, boxX2 = box.x + box.w, boxY1 = box.y, boxY2 = box.y + box.h;
        // Calculate the value of currentLesserOffset, given the length of the pattern that we have just traversed.
        function recalculateOffset(length) {
            // If there's an absurdly long segment, we just pretend the length is 0 to avoid problems with Infinities/NaNs
            if (length > 1e6)
                length = 0;
            // Move length along the dashOffset, modulo the patternLength
            dashOffset += length;
            dashOffset %= patternLength;
            // It's certainly possible to precompute these sums and use a binary search to find the dash index, but
            // that's unnecessary for dashes with short length
            let sum = 0, i = 0, lesserOffset = 0;
            for (; i < dashPattern.length; ++i) {
                let dashLength = dashPattern[i];
                // Accumulate the length from the start of the pattern to the current dash
                sum += dashLength;
                // If the dashOffset is within this dash...
                if (dashOffset <= sum) {
                    // calculate the lesser offset
                    lesserOffset = dashOffset - sum + dashLength;
                    break;
                }
            }
            // Set the current index and lesserOffset
            currentIndex = i;
            currentLesserOffset = lesserOffset;
        }
        // Generate dashes for the line segment (x1, y1) -- (x2, y2)
        function generateDashes(x1, y1, x2, y2) {
            // length of the segment
            const length = fastHypot(x2 - x1, y2 - y1);
            // index of where along the dashes we are
            let i = currentIndex;
            // Length so far of emitted dashes
            let lengthSoFar = 0;
            // We do this instead of while (true) to prevent the program from crashing
            for (let _ = 0; _ < MAX_DASHED_POLYLINE_VERTICES; _++) {
                // Length of the dash/gap component we need to draw (we subtract currentLesserOffset because that is already drawn)
                const componentLen = dashPattern[i] - currentLesserOffset;
                // Length when this component ends
                const endingLen = componentLen + lengthSoFar;
                // Whether we are in a dash
                const inDash = i % 2 === 0;
                if (endingLen <= length) {
                    // If the end of the dash/gap occurs before the end of the current segment, we need to continue
                    let r = endingLen / length;
                    // if in a gap, this starts the next dash; if in a dash, this ends the dash
                    result.push(x1 + (x2 - x1) * r, y1 + (y2 - y1) * r);
                    // If we're ending off a dash, put the gap in
                    if (inDash)
                        result.push(NaN, NaN);
                    // Go to the next dash/gap
                    ++i;
                    i %= dashPattern.length;
                    // Reset the current lesser offset
                    currentLesserOffset = 0;
                }
                else {
                    // If we're in a dash, that means we're in the middle of a dash, so we just add the vertex
                    if (inDash)
                        result.push(x2, y2);
                    break;
                }
                lengthSoFar += componentLen;
            }
            // Recalculate currentLesserOffset
            recalculateOffset(length);
        }
        if (currentIndex % 2 === 0)
            // We're beginning with a dash, so start it off
            result.push(vertices[0], vertices[1]);
        for (let i = 0; i < vertices.length - 2; i += 2) {
            // For each pair of vertices...
            let x1 = vertices[i];
            let y1 = vertices[i + 1];
            let x2 = vertices[i + 2];
            let y2 = vertices[i + 3];
            if (Number.isNaN(x1) || Number.isNaN(y1)) {
                // At the start of every subpolyline, reset the dash offset
                dashOffset = pen.dashOffset;
                // Recalculate the initial currentLesserOffset
                recalculateOffset(0);
                // End off the previous subpolyline
                result.push(NaN, NaN);
                continue;
            }
            // If the end of the segment is undefined, continue
            if (Number.isNaN(x2) || Number.isNaN(y2))
                continue;
            // Length of the segment
            let length = fastHypot(x2 - x1, y2 - y1);
            // Find whether the segment intersects the box
            let intersect = lineSegmentIntersectsBox(x1, y1, x2, y2, boxX1, boxY1, boxX2, boxY2);
            // If the segment doesn't intersect the box, it is entirely outside the box, so we can add its length to pretend
            // like we drew it even though we didn't
            if (!intersect) {
                recalculateOffset(length);
                continue;
            }
            // Whether (x1, y1) and (x2, y2) are contained within the box
            let pt1Contained = intersect[0] === x1 && intersect[1] === y1;
            let pt2Contained = intersect[2] === x2 && intersect[3] === y2;
            // If (x1, y1) is contained, fake draw the portion of the line outside of the box
            if (!pt1Contained)
                recalculateOffset(fastHypot(x1 - intersect[0], y1 - intersect[1]));
            // Generate dashes
            generateDashes(intersect[0], intersect[1], intersect[2], intersect[3]);
            if (!pt2Contained)
                recalculateOffset(fastHypot(x2 - intersect[2], y2 - intersect[3]));
            if (result.length > MAX_DASHED_POLYLINE_VERTICES)
                throw new Error('Too many generated vertices in getDashedPolyline.');
        }
        return result;
    }

    class VertexData {
        constructor(vertices, vertexCount, dim, version = -1) {
            this.vertices = vertices;
            this.vertexCount = vertexCount;
            this.dim = dim;
            this.version = version;
        }
    }

    const MIN_RES_ANGLE = 0.05; // minimum angle in radians between roundings in a polyline
    const B = 4 / Math.PI;
    const C = -4 / Math.PI ** 2;
    function fastSin(x) {
        // crude, but good enough for this
        x %= 6.28318530717;
        if (x < -3.14159265)
            x += 6.28318530717;
        else if (x > 3.14159265)
            x -= 6.28318530717;
        return B * x + C * x * (x < 0 ? -x : x);
    }
    function fastCos(x) {
        return fastSin(x + 1.570796326794);
    }
    function fastAtan2(y, x) {
        let abs_x = x < 0 ? -x : x;
        let abs_y = y < 0 ? -y : y;
        let a = abs_x < abs_y ? abs_x / abs_y : abs_y / abs_x;
        let s = a * a;
        let r = ((-0.0464964749 * s + 0.15931422) * s - 0.327622764) * s * a + a;
        if (abs_y > abs_x)
            r = 1.57079637 - r;
        if (x < 0)
            r = 3.14159265 - r;
        if (y < 0)
            r = -r;
        return r;
    }
    /**
     * Convert an array of polyline vertices into a Float32Array of vertices to be rendered using WebGL.
     * @param vertices {Array} The vertices of the polyline.
     * @param pen {Object} A JSON representation of the pen. Could also be the pen object itself.
     * @param box {BoundingBox} The bounding box of the plot, used to optimize line dashes
     */
    function calculatePolylineVertices(vertices, pen, box = null) {
        if (pen.dashPattern.length === 0) {
            return convertTriangleStrip(vertices, pen);
        }
        else {
            return convertTriangleStrip(getDashedPolyline(vertices, pen, box), pen);
        }
    }
    function convertTriangleStrip(vertices, pen) {
        if (pen.thickness <= 0 ||
            pen.endcapRes < MIN_RES_ANGLE ||
            pen.joinRes < MIN_RES_ANGLE ||
            vertices.length <= 3) {
            return { vertices: null, vertexCount: 0, dim: 2 };
        }
        let index = -1;
        let origVertexCount = vertices.length / 2;
        let th = pen.thickness / 2;
        let maxMiterLength = th / fastCos(pen.joinRes / 2);
        let endcap = pen._toEndcapTypeEnum();
        let join = pen._toJoinTypeEnum();
        let endcapRes = pen.endcapRes;
        let joinRes = pen.joinRes;
        let maxVerticesPerStep = 2 + Math.max(6 * Math.PI / endcapRes, 6 * Math.PI / joinRes) | 0;
        // Grows by ceiling multiples of 1.5, a la most implementations of a vector
        // We begin with a very rough estimate of how much space will be needed
        let glVertices = new Float32Array(origVertexCount * 2 + // raw minimum
            ((endcap === 1) ? (2.1 * Math.PI / endcapRes) : 0) + // rough endcap minimum
            ((join === 1 || join === 3) ? (origVertexCount * 0.1 / joinRes) : 0) + // join estimate, assuming an average of 0.1 radians per join
            maxVerticesPerStep);
        function reallocGLVertices(minSize) {
            minSize = minSize | 0;
            if (minSize > glVertices.length) {
                minSize = (minSize * 1.5) | 0;
                let newGLVertices = new Float32Array(minSize);
                // Copy it over
                newGLVertices.set(glVertices, 0);
                glVertices = newGLVertices;
            }
        }
        // p1 -- p2 -- p3, generating vertices for point p2
        let x1 = 0, x2 = 0, x3 = vertices[0], y1 = 0, y2 = 0, y3 = vertices[1];
        let v1x = 0, v1y = 0, v2x = 0, v2y = 0, v1l = 0, v2l = 0, b1_x = 0, b1_y = 0, scale = 0, dis = 0;
        for (let i = 0; i < origVertexCount; ++i) {
            reallocGLVertices(index + maxVerticesPerStep);
            x1 = i !== 0 ? x2 : NaN; // Previous vertex
            x2 = x3; // Current vertex
            x3 = i !== origVertexCount - 1 ? vertices[2 * i + 2] : NaN; // Next vertex
            y1 = i !== 0 ? y2 : NaN; // Previous vertex
            y2 = y3; // Current vertex
            y3 = i !== origVertexCount - 1 ? vertices[2 * i + 3] : NaN; // Next vertex
            if (Math.abs(x3) > 16384 || Math.abs(y3) > 16384) {
                // Temporary
                x3 = NaN;
                y3 = NaN;
            }
            if (x2 !== x2 || y2 !== y2) {
                continue;
            }
            if (x1 !== x1 || y1 !== y1) {
                // The start of every endcap has two duplicate vertices for triangle strip reasons
                v2x = x3 - x2;
                v2y = y3 - y2;
                v2l = fastHypot(v2x, v2y);
                if (v2l < 1e-8) {
                    v2x = 1;
                    v2y = 0;
                }
                else {
                    v2x /= v2l;
                    v2y /= v2l;
                }
                if (v2x !== v2x || v2y !== v2y) {
                    continue;
                } // undefined >:(
                if (endcap === 1) {
                    // rounded endcap
                    let theta = fastAtan2(v2y, v2x) + Math.PI / 2;
                    let steps_needed = Math.ceil(Math.PI / endcapRes);
                    let o_x = x2 - th * v2y, o_y = y2 + th * v2x;
                    let theta_c = theta + (1 / steps_needed) * Math.PI;
                    // Duplicate first vertex
                    let x = glVertices[++index] = x2 + th * fastCos(theta_c);
                    let y = glVertices[++index] = y2 + th * fastSin(theta_c);
                    glVertices[++index] = x;
                    glVertices[++index] = y;
                    glVertices[++index] = o_x;
                    glVertices[++index] = o_y;
                    for (let i = 2; i <= steps_needed; ++i) {
                        let theta_c = theta + (i / steps_needed) * Math.PI;
                        glVertices[++index] = x2 + th * fastCos(theta_c);
                        glVertices[++index] = y2 + th * fastSin(theta_c);
                        glVertices[++index] = o_x;
                        glVertices[++index] = o_y;
                    }
                    continue;
                }
                else if (endcap === 2) {
                    let x = glVertices[++index] = x2 - th * v2x + th * v2y;
                    let y = glVertices[++index] = y2 - th * v2y - th * v2x;
                    glVertices[++index] = x;
                    glVertices[++index] = y;
                    glVertices[++index] = x2 - th * v2x - th * v2y;
                    glVertices[++index] = y2 - th * v2y + th * v2x;
                    continue;
                }
                else {
                    // no endcap
                    let x = glVertices[++index] = x2 + th * v2y;
                    let y = glVertices[++index] = y2 - th * v2x;
                    glVertices[++index] = x;
                    glVertices[++index] = y;
                    glVertices[++index] = x2 - th * v2y;
                    glVertices[++index] = y2 + th * v2x;
                    continue;
                }
            }
            if (x3 !== x3 || y3 !== y3) {
                // ending endcap
                v1x = x2 - x1;
                v1y = y2 - y1;
                v1l = v2l;
                if (v1l < 1e-8) {
                    v1x = 1;
                    v1y = 0;
                }
                else {
                    v1x /= v1l;
                    v1y /= v1l;
                }
                if (v1x !== v1x || v1y !== v1y) {
                    continue;
                } // undefined >:(
                glVertices[++index] = x2 + th * v1y;
                glVertices[++index] = y2 - th * v1x;
                glVertices[++index] = x2 - th * v1y;
                glVertices[++index] = y2 + th * v1x;
                if (endcap === 1) {
                    let theta = fastAtan2(v1y, v1x) + (3 * Math.PI) / 2;
                    let steps_needed = Math.ceil(Math.PI / endcapRes);
                    let o_x = x2 - th * v1y, o_y = y2 + th * v1x;
                    for (let i = 1; i <= steps_needed; ++i) {
                        let theta_c = theta + (i / steps_needed) * Math.PI;
                        glVertices[++index] = x2 + th * fastCos(theta_c);
                        glVertices[++index] = y2 + th * fastSin(theta_c);
                        glVertices[++index] = o_x;
                        glVertices[++index] = o_y;
                    }
                }
                // Duplicate last vertex of ending endcap
                glVertices[index + 1] = glVertices[index - 1];
                glVertices[index + 2] = glVertices[index];
                index += 2;
                continue;
            }
            // all vertices are defined, time to draw a joinerrrrr
            if (join === 2 || join === 3) {
                // find the two angle bisectors of the angle formed by v1 = p1 -> p2 and v2 = p2 -> p3
                v1x = x1 - x2;
                v1y = y1 - y2;
                v2x = x3 - x2;
                v2y = y3 - y2;
                v1l = v2l;
                v2l = fastHypot(v2x, v2y);
                b1_x = v2l * v1x + v1l * v2x;
                b1_y = v2l * v1y + v1l * v2y;
                scale = 1 / fastHypot(b1_x, b1_y);
                if (scale === Infinity || scale === -Infinity) {
                    b1_x = -v1y;
                    b1_y = v1x;
                    scale = 1 / fastHypot(b1_x, b1_y);
                }
                b1_x *= scale;
                b1_y *= scale;
                scale = (th * v1l) / (b1_x * v1y - b1_y * v1x);
                if (join === 2 || Math.abs(scale) < maxMiterLength) {
                    // Draw a miter. But the length of the miter is massive and we're in dynamic mode (3), we exit this if statement and do a rounded join
                    b1_x *= scale;
                    b1_y *= scale;
                    glVertices[++index] = x2 - b1_x;
                    glVertices[++index] = y2 - b1_y;
                    glVertices[++index] = x2 + b1_x;
                    glVertices[++index] = y2 + b1_y;
                    continue;
                }
            }
            v2x = x3 - x2;
            v2y = y3 - y2;
            dis = fastHypot(v2x, v2y);
            if (dis < 0.001) {
                v2x = 1;
                v2y = 0;
            }
            else {
                v2x /= dis;
                v2y /= dis;
            }
            v1x = x2 - x1;
            v1y = y2 - y1;
            dis = fastHypot(v1x, v1y);
            if (dis === 0) {
                v1x = 1;
                v1y = 0;
            }
            else {
                v1x /= dis;
                v1y /= dis;
            }
            glVertices[++index] = x2 + th * v1y;
            glVertices[++index] = y2 - th * v1x;
            glVertices[++index] = x2 - th * v1y;
            glVertices[++index] = y2 + th * v1x;
            if (join === 1 || join === 3) {
                let a1 = fastAtan2(-v1y, -v1x) - Math.PI / 2;
                let a2 = fastAtan2(v2y, v2x) - Math.PI / 2;
                // if right turn, flip a2
                // if left turn, flip a1
                let start_a, end_a;
                if (mod(a1 - a2, 2 * Math.PI) < Math.PI) {
                    // left turn
                    start_a = Math.PI + a1;
                    end_a = a2;
                }
                else {
                    start_a = Math.PI + a2;
                    end_a = a1;
                }
                let angle_subtended = mod(end_a - start_a, 2 * Math.PI);
                let steps_needed = Math.ceil(angle_subtended / joinRes);
                for (let i = 0; i <= steps_needed; ++i) {
                    let theta_c = start_a + (angle_subtended * i) / steps_needed;
                    glVertices[++index] = x2 + th * fastCos(theta_c);
                    glVertices[++index] = y2 + th * fastSin(theta_c);
                    glVertices[++index] = x2;
                    glVertices[++index] = y2;
                }
            }
            glVertices[++index] = x2 + th * v2y;
            glVertices[++index] = y2 - th * v2x;
            glVertices[++index] = x2 - th * v2y;
            glVertices[++index] = y2 + th * v2x;
        }
        return new VertexData((index >= 0) ? glVertices : null, index >> 1, 2, getVersionID());
    }

    class PolylineElement extends Element {
        init(params) {
        }
        /**
         * Set the pen, merging properties. For example, setting the pen color, then the thickness, will not reset the color.
         * @param pen
         */
        setPen(pen) {
            this.props.set("pen", Pen.compose(Pen.default(), pen));
            return this;
        }
        _getPen() {
            let p = this.props.get("pen");
            if (!p)
                this.props.set("pen", p = Pen.default());
            return p;
        }
        getPen() {
            return Pen.create(this._getPen());
        }
        setColor(color) {
            this.props.get("pen").color = Color.fromObj(color);
            this.props.markChanged("pen");
            return this;
        }
        /**
         * Set the vertices, which will be converted to a flat array. Setting to undefined will clear the vertices.
         * @param v
         * @param f32 Whether to convert to single-precision to save space
         */
        setVertices(v, f32 = true) {
            this.props.set("vertices", v ? vec2ArrayConversion(v, f32, true) : v);
        }
        /**
         * Returns a flat array of the vertices
         */
        getVertices() {
            return this.props.get("vertices");
        }
        _computeDrawVertices() {
            let setDrawVertices = (v) => {
                this.internal.triangulation = v;
            };
            let vertices = this.getVertices();
            let pen = this.getPen();
            this.internal.drawPen = pen; // cloned
            if (!vertices || !(pen instanceof Pen)) {
                setDrawVertices(null);
                return;
            }
            let drawVertices = calculatePolylineVertices(vertices, pen);
            setDrawVertices(drawVertices);
        }
        getBoundingBox() {
        }
        _computeBoundingBox() {
        }
        _update() {
            // The algorithm is as follows:
            // If pen has changed and only the color has changed, we recompute nothing.
            let props = this.props;
            let pen = this._getPen();
            let needsRecompute = true;
            if (!props.hasChanged("vertices")) {
                // Vertices haven't changed, check if the pen has changed
                if (!props.hasChanged("pen"))
                    return; // nothing changed
                let previousPen = this.internal.drawPen;
                if (previousPen) {
                    previousPen.color = pen.color;
                    if (previousPen.equals(pen)) { // pens are the same except for color; vertices don't need recomputation
                        needsRecompute = false;
                    }
                }
            }
            if (needsRecompute)
                this._computeDrawVertices();
            this.internal.drawPen = pen;
            let vertexData = this.internal.triangulation;
            this.internal.renderInfo = vertexData ? {
                instructions: [
                    {
                        insnType: "primitive",
                        primitiveType: "triangle_strip",
                        vertexData,
                        pen: pen,
                        version: getVersionID()
                    }
                ]
            } : null;
        }
        clone() {
            let e = new PolylineElement();
            e.setPen(this._getPen());
            e.setVertices(this.getVertices());
            // TODO copy precomputed information?
            return e;
        }
    }

    const MIN_ASPECT = 1e-300;
    const MAX_ASPECT = 1e300;
    /**
     * Plot2D abstracts, well, a 2D plot with some transform, with certain boundaries and a certain
     * transform (currently always linear, but that may change at some point).
     *
     * Children of a Plot2D inherit a plot transform and some other plot configuration properties.
     * A Plot2D has a width and height that may be determined automatically or set by the user.
     */
    class Plot2D extends Group {
        init(params) {
            let props = this.props;
            // Inherits all the way down
            props.setPropertyInheritance("plotTransform", true);
            props.set("plotTransform", new LinearPlot2DTransform());
        }
        setPreserveAspectRatio(v) {
            v = !!v;
            this.props.set("preserveAspectRatio", v);
        }
        setAspectRatio(v) {
            if (v !== v || v >= MIN_ASPECT || v <= MAX_ASPECT) {
                throw new RangeError("invalid aspect ratio");
            }
            this.props.set("aspectRatio", v);
        }
        getPreserveAspectRatio() {
            return this.props.get("preserveAspectRatio");
        }
        getAspectRatio() {
            return this.props.get("aspectRatio");
        }
        getTransform() {
            return this.props.get("plotTransform");
        }
        // For now
        resizeToFit(boxlike) {
            let box = BoundingBox.fromObj(boxlike);
            let transform = this.getTransform().clone();
            transform.resizeToPixelBox(box);
            this.props.set("plotTransform", transform);
            this.correctAspectRatio();
        }
        _update() {
            this.updateTransform();
        }
        updateTransform() {
            this.correctAspectRatio();
        }
        /**
         * Correct the graph's current aspect ratio to what it is supposed to be
         * @param eps
         */
        correctAspectRatio() {
            // Fix the aspect ratio of the plot transform
            let shouldPreserve = this.getPreserveAspectRatio();
            if (!shouldPreserve)
                return;
            let aspectRatio = this.getAspectRatio();
            let transform = this.getTransform();
            let currentAspect = transform.getGraphAspectRatio();
            // Close enough...
            if (Math.abs(currentAspect / aspectRatio - 1) < 0.001)
                return;
            // TODO
        }
        fitScene() {
            let size = this.props.get("sceneDims");
            if (!size)
                throw new Error("not child of scene");
            this.resizeToFit(size.getBoundingBox());
        }
    }

    exports.BigFloat = BigFloat;
    exports.BolusCancellationError = BolusCancellationError;
    exports.BolusTimeoutError = BolusTimeoutError;
    exports.Color = Color;
    exports.Colors = Colors;
    exports.CompilationError = CompilationError;
    exports.Complex = Complex;
    exports.InteractiveScene = InteractiveScene;
    exports.LinearPlot2DTransform = LinearPlot2DTransform;
    exports.ParserError = ParserError;
    exports.Pen = Pen;
    exports.Plot2D = Plot2D;
    exports.PolylineElement = PolylineElement;
    exports.ROUNDING_MODE = ROUNDING_MODE;
    exports.RealInterval = RealInterval;
    exports.StandardColoringScheme = StandardColoringScheme;
    exports.Vec2 = Vec2;
    exports.WASM = WASM;
    exports.WebGLRenderer = WebGLRenderer;
    exports._boundingBoxFlatF32 = _boundingBoxFlatF32;
    exports.addMantissas = addMantissas;
    exports.asyncDigest = asyncDigest;
    exports.combineColoredTriangleStrips = combineColoredTriangleStrips;
    exports.combineTriangleStrips = combineTriangleStrips;
    exports.compileNode = compileNode;
    exports.computeBoundingBox = computeBoundingBox;
    exports.countFloatsBetween = countFloatsBetween;
    exports.fastAtan2 = fastAtan2$1;
    exports.fastHypot = fastHypot;
    exports.fillRepeating = fillRepeating;
    exports.flattenVec2Array = flattenVec2Array;
    exports.floatStore = floatStore;
    exports.flrLog2 = flrLog2;
    exports.frExp = frExp;
    exports.gammaReal = gammaReal;
    exports.genVariableName = genVariableName;
    exports.generateCircleTriangleStrip = generateCircleTriangleStrip;
    exports.generateRectangleCycle = generateRectangleCycle;
    exports.generateRectangleDebug = generateRectangleDebug;
    exports.generateRectangleTriangleStrip = generateRectangleTriangleStrip;
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
    exports.lineSegmentIntersectsBox = lineSegmentIntersectsBox;
    exports.lnGammaReal = lnGammaReal;
    exports.mantissaClz = mantissaClz;
    exports.mantissaCtz = mantissaCtz;
    exports.neededWordsForPrecision = neededWordsForPrecision;
    exports.parseString = parseString;
    exports.pointInTriangle = pointInTriangle;
    exports.pow2 = pow2;
    exports.prettyPrintMantissa = prettyPrintMantissa;
    exports.rationalExp = rationalExp;
    exports.resolveOperatorDefinition = resolveOperatorDefinition;
    exports.riemannZetaReal = riemannZetaReal;
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
    exports.vec2ArrayConversion = vec2ArrayConversion;

    Object.defineProperty(exports, '__esModule', { value: true });

})));
