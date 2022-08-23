import {BigFloat, WORKING_RM, WORKING_PRECISION} from "./bigfloat.js";
import {FastBooleanInterval} from "../bool/fast_interval.js";
import {flrLog2, pow2} from "../fp/manip.js";

/**
 * Approximate radius by number
 * @param re
 * @param rk
 */
function radiusToNumber (re: number, rk: number): number {
    if (re < -1000) { // avoid underflow
        return pow2(re + 30) * rk / 0x40000000
    }

    return pow2(re) * rk
}

// Saturating shift a 30-bit number right, rounding up
function sshiftRndUp(n: number, shift: number): number {
    shift = +shift
    n = n | 0

    if (shift >= 30) {
        return +(n > 0)
    }

    let msk = (1 << shift) - 1

    return (n >> shift) + +(msk > 0)
}

function addRadii (re1: number, rk1: number, re2: number, rk2: number, re3: number, rk3: number, target: BigBall) {
    let re = rk1 + rk2 + rk3
    if (!Number.isFinite(re)) {  // cheeky way of handling the special cases
        target.re = re
        target.rk = 0
        return
    }

    rk1 = rk1 | 0
    rk2 = rk2 | 0
    rk3 = rk3 | 0

    // Order by exponent
    function swpRe23() {
        if (re3 > re2) {
            let tmp = re2; re2 = re3; re3 = tmp
            let tmp2 = rk2; rk2 = rk3; rk3 = tmp2
        }
    }

    swpRe23()
    if (re2 > re1) {
        let tmp = re1; re1 = re2; re2 = tmp
        let tmp2 = rk1; rk1 = rk2; rk2 = tmp2
    }
    swpRe23()

    rk2 = (rk2 + sshiftRndUp(rk3, re2 - re3)) | 0
    rk1 = (rk1 + sshiftRndUp(rk2, re1 - re2)) | 0

    while (rk1 >= 1 << 30) {
        rk1 = ((rk1 >> 1) + (rk1 & 1)) | 0  // round up shift right one
        re1 = re1 + 1
    }

    if (re1 > MAX_RE) {
        target.re = NaN
        target.rk = 0
    } else {
        target.re = re1
        target.rk = rk1
    }
}

function mulRadii1(re1: number, rk1: number, re2: number, rk2: number, target: BigBall) {
    let rk1lw = rk1 & 0x7fff, rk1hw = rk1 >> 15, rk2lw = rk2 & 0x7fff, rk2hw = rk2 >> 15
    let hi = Math.imul(rk1hw, rk2hw)
    let mid = Math.imul(rk1lw, rk2hw) + Math.imul(rk1hw, rk2lw)
    let lo = Math.imul(rk1lw, rk2lw)

    mid = (mid + (lo >> 15) + +!!(lo & 0x7fff)) | 0

    let sh = 2 - Math.clz32(hi)

    re1 = re1 - sh
    hi = ((hi << sh) + (mid >> (15 - sh)) + +!!(mid & ((0x8000 << sh) - 1)))

    let k = +hi

    if (re1 < MIN_RE) {
        re1 = MIN_RE
        k = MIN_RK
    } else if (re1 > MAX_RE) {
        re1 = 0

    }
}

const MIN_RE = -4503599627370495, MAX_RE = 4503599627370495, MIN_RK = 1 << 29, MAX_RK = (1 << 30) - 1

type NormalizeRadiusError = 0 | 1

/**
 * Assumes rk > 0, isFinite(re, rk). Rounds up. Returns 1 if the new radius is inexact.
 * @param re
 * @param rk
 * @param bb
 */
function normalizeRadiusSet(re: number, rk: number, bb: BigBall): NormalizeRadiusError {
    let offs = 29 - flrLog2(rk), inexact: NormalizeRadiusError = 0

    re -= offs
    let rkn = pow2(offs)

    // round rk up to next integer in [1 << 29, (1 << 30) - 1]
    rk = Math.ceil(rkn)
    inexact = +(rk !== rkn) as NormalizeRadiusError

    if (rk === 1 << 30) {
        rk = MIN_RK
        re += 1
    }

    if (re > MAX_RE) {
        rk = NaN
        re = 0
        inexact = 1
    } else if (re < MIN_RE) {
        // Round up to minimum radius
        rk = MIN_RE
        re = 1
        inexact = 1
    }

    bb.re = re
    bb.rk = rk

    return inexact
}

// rk should be between 1 << 29 and (1 << 30) - 1, inclusive

type OneSidedBigBallDirection = -1 | 1

// A "big ball" is an arbitrary-precision interval. There are four states in which a big ball can exist: all numbers,
// signified by an exponent of NaN; (-inf, c], signified by an exponent of -Infinity; [c, inf), signified by an exponent
// of Infinity; and [c - r, c + r], a plain-old ball. Note that r can equal zero, for exact cases.

// Big ball operations generally use "whatever"-mode rounding, for performance reasons. The very point of big balls is
// to be slightly sloppy, but in a rigorous way, and fast. For certain cases, exact rounding may be used.
export class BigBall {
    // center of the big ball
    c: BigFloat

    // Exponent of the radius (base 2, not 2^30).
    re: number
    // Multiple of 2^re for the radius
    rk: number

    constructor(prec: number, re: number, rk: number) {
        this.c = BigFloat.new(prec)
        this.setRadiusComponents(re, rk)
    }

    static new(prec: number=WORKING_PRECISION): BigBall {
        return new BigBall(prec, 0, 0)
    }

    static newFromNumber(n: number, prec: number=WORKING_PRECISION): BigBall {
        let ball = BigBall.new(prec)

        ball.setFromNumber(n)

        return ball
    }

    setFromNumber(n: number) {
        this.c.setFromNumber(n)

        this.re = 0
        if (!Number.isFinite(n)) {
            this.rk = NaN
        } else {
            this.rk = 0
        }
    }

    /**
     * Set this big ball's radius to 2^re * rk. These values must be valid
     * @param re
     * @param rk
     */
    setRadiusUnchecked(re: number, rk: number) {
        this.re = re
        this.rk = rk
    }

    setRadiusZero() {
        this.re = 0
        this.rk = 0
    }

    setRadiusInf() {
        this.re = 0
        this.rk = NaN
    }

    setZero() {
        this.re = 0
        this.rk = 0

        this.c.setZero()
    }

    /**
     * Set the direction of the big ball for infinite radii. A negative number goes to -inf; a positive number goes to
     * +inf.
     * @param dir
     */
    setRadiusOneSided(dir: OneSidedBigBallDirection) {
        this.re = 0
        this.rk = dir * Infinity
    }

    /**
     * Whether this big ball is all real numbers
     */
    isInf(): boolean {
        let rk = this.rk
        return rk !== rk
    }

    isOneSided(): boolean {
        return Math.abs(this.rk) === Infinity
    }

    isSpecial(): boolean {
        return Number.isFinite(this.rk)
    }

    setRadiusNumber(x: number) {
        this.setRadiusComponents(0, x)
    }

    setRadiusComponents(re: number, rk: number) {
        if (re === -Infinity || rk === 0) {
            this.setRadiusZero()
        } else if (re === Infinity || re !== re || rk !== rk) {
            this.setRadiusInf()
        } else {
            // Normalize re/rk
            normalizeRadiusSet(re, rk, this)
        }
    }


    static addTo(b1: BigBall, b2: BigBall, target: BigBall) {

    }
}

const SCRATCH_BIGBALL1 = BigBall.new(60)
const SCRATCH_BIGBALL2 = BigBall.new(60)