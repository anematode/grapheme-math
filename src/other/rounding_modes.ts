export const ROUNDING_MODE = {
  WHATEVER: 0, // do whatever's fastest (including no rounding at all), guaranteeing a result with error <= 1 ulp
  NEAREST: 0b10, // nearest neighbor, ties to even
  TIES_EVEN: 0b10,
  TIES_ODD: 0b110,
  TIES_AWAY: 0b10010, // tie away from zero (toward ±inf)
  TIES_ZERO: 0b10011, // tie toward zero
  UP: 0b10000,
  DOWN: 0b10001,
  TOWARD_INF: 0b110000, // toward the extremes
  TOWARD_ZERO: 0b110001 // toward zero
} as const

Object.freeze(ROUNDING_MODE)

export type RoundingMode = typeof ROUNDING_MODE[keyof typeof ROUNDING_MODE]

// Bitfield:
// bit 0: 0 -> whatever or going up in some way, 1 -> going down in some way
// bit 1: 0 -> not ties, 1 -> ties
// bit 2: distinguish ties even, ties odd
// bit 3: unused
// bit 4: has different behavior when flipped in sign (should use ^= 1)
// bit 5: is toward inf or toward zero

// Simple checking for behavior in mantissa functions
// if (!rm) { /* whatever */ }
// else if (rm & 2) { /* ties */ }
// else if (!(rm & 1)) { /* inf or up */ }
// else { /* zero or down */ }

// Flipping rounding modes when necessary
// if (rm & 16 && flip) { /* up or down, ties away or ties zero, toward inf or toward zero */ rm ^= 1 }

// Check if (up or down)
// if (rm & 16 && rm < 0b10010)
// Check if (toward inf or toward zero)
// if (rm & 32)
// Check if (ties)
// if (rm & 2)

/**
 * Convert a numeric rounding mode to a string
 * @param mode Any number; throws on invalid
 */
export function roundingModeToString (mode: number): string {
  switch (mode) {
    case ROUNDING_MODE.WHATEVER:
      return 'WHATEVER'
    case ROUNDING_MODE.NEAREST:
      return 'TIES_EVEN'
    case ROUNDING_MODE.TIES_ODD:
      return 'TIES_ODD'
    case ROUNDING_MODE.TIES_AWAY:
      return 'TIES_AWAY'
    case ROUNDING_MODE.TIES_ZERO:
      return 'TIES_ZERO'
    case ROUNDING_MODE.UP:
      return 'UP'
    case ROUNDING_MODE.DOWN:
      return 'DOWN'
    case ROUNDING_MODE.TOWARD_INF:
      return 'TOWARD_INF'
    case ROUNDING_MODE.TOWARD_ZERO:
      return 'TOWARD_ZERO'
  }

  throw new TypeError(`Invalid rounding mode`)
}

/**
 * Whether a given object is a recognized rounding mode
 * @param n Any object
 * @returns Whether it is a valid rounding mode
 */
export function isRoundingMode (n: any): boolean {
  if (typeof n !== "number") return false

  return (n === 0 || n === 0b10 || n === 0b110 || n === 0b10010 || n === 0b10011 || n === 0b10000 || n === 0b10001 || n === 0b110000 || n === 0b110001)
}

/**
 * Flip a rounding mode (up to down, toward inf to toward zero)
 * @param n Rounding mode
 * @returns Flipped rounding mode
 */
export function flipRoundingMode (n: RoundingMode): RoundingMode {
  return ((n & 16) ? (n ^ 1) : n) as RoundingMode
}

