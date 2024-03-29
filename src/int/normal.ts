/**
 * Integers are interesting, in that ideally we want to catch overflow (ex., 3^100 will not be represented exactly), but
 * in many contexts that overflow isn't what we care about, and we just want float values. For now, we will let integers
 * be anything except non-integer finite values. In other words, we permit integers to be -Infinity, Infinity, NaN, and
 * any integer, but not something like 0.5. We include -0.
 */

const builtinIsInteger = Number.isInteger
const builtinIsFinite = Number.isFinite

/**
 * Check if a parameter is a valid nullable integer (must be an integer number or NaN, ±Infinity)
 * @param i Anything
 * @returns Whether the given parameter is a valid nullable integer
 */
function isNullableInteger (i: any): boolean {
  return builtinIsInteger(i) || (typeof i === "number" && !builtinIsFinite(i))
}

/**
 * Return a descriptive error message if a number is not a valid nullable integer; otherwise, return the empty string
 * @param i Anything
 * @returns Error message if the parameter is an invalid nullable integer; otherwise, an empty message
 */
function typecheckNullableInteger (i: any): string {
  let isInteger = builtinIsInteger(i)
  if (isInteger) return ""

  if (typeof i !== "number")
    return "Expected nullable integer (integer, ±Infinity, or NaN), found type " + (typeof i)

  let isFinite = builtinIsFinite(i)

  if (!isFinite)
    return "Expected nullable integer, found non-integral number " + i

  return ""
}

const NullableInteger = Object.freeze({
  isNullableInteger,
  typecheckNullableInteger
})

export { NullableInteger }
