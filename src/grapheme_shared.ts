// File shared between all modules of Grapheme
let version = 0

/**
 * This function returns a number starting from 1 that never decreases. It is used to store "when" an operation has
 * occurred, and thus whether to consider it a change.
 * @returns {number}
 */
export function getVersionID () {
  return ++version
}

// Generate an id of the form xxxx-xxxx
// TODO: guarantee no collisions via LFSR or something similar
export function getStringID () {
  function randLetter () {
    return String.fromCharCode(Math.round(Math.random() * 25 + 97))
  }

  function randFourLetter () {
    return randLetter() + randLetter() + randLetter() + randLetter()
  }

  return randFourLetter() + '-' + randFourLetter()
}

/**
 * Simple deep equals. Uses Object.is-type equality, though. Doesn't handle circularity or any of the fancy new containers
 * @param x {*}
 * @param y {*}
 * @param lookForEqualsMethod {boolean} Whether to look for a method "equals()" on x to use instead of the standard method of comparison
 * @returns {boolean}
 */
export function deepEquals (x, y, lookForEqualsMethod=false): boolean {
  if (typeof x !== 'object' || x === null) return Object.is(x, y)

  if (lookForEqualsMethod && x.equals) return x.equals(y)
  if (x.constructor !== y.constructor) return false

  if (Array.isArray(x) && Array.isArray(y)) {
    if (x.length !== y.length) return false
    for (let i = x.length - 1; i >= 0; --i) {
      if (!deepEquals(x[i], y[i], lookForEqualsMethod)) return false
    }

    return true
  }

  // The only other thing of consequence to us. Could probably handle other weird objects too, but meh.
  if (isTypedArray(x) && isTypedArray(y)) {
    if (x.length !== y.length) return false

    if (x instanceof Float32Array || x instanceof Float64Array) {
      for (let i = x.length - 1; i >= 0; --i) {
        const xv = x[i]

        // What a beautiful way to test for same valueness between floats!
        if (
          (xv !== y[i] && !(xv !== xv && y[i] !== y[i])) ||
          (xv === 0 && 1 / xv !== 1 / y[i])
        )
          return false
      }
    } else {
      for (let i = x.length - 1; i >= 0; --i) {
        if (x[i] !== y[i]) return false
      }
    }

    return true
  }

  if (x instanceof Map || x instanceof Set) return false // Just in case

  // x and y are just objects
  const keys = Object.keys(x)
  if (Object.keys(y).length !== keys.length) return false

  for (const key of keys) {
    // fails if y is Object.create(null)
    if (!y.hasOwnProperty(key)) return false
    if (!deepEquals(x[key], y[key])) return false
  }

  return true
}

/**
 * Euclidean algorithm. Only works for suitably small a and b.
 * @param a
 * @param b
 * @returns {number}
 */
export function gcd (a: number, b: number): number {
  a = Math.abs(a)
  b = Math.abs(b)

  if (b > a) {
    let tmp = a
    a = b
    b = tmp
  }

  while (true) {
    if (b === 0) return a
    a %= b
    if (a === 0) return b
    b %= a
  }
}

type DeepAssignOptions = {
  // Whether to deep clone arrays
  cloneArrays?: boolean
  // Whether to copy undefined options
  assignUndefined?: boolean
}

type DeepAssignFilledOptions = {
  cloneArrays: boolean
  assignUndefined: boolean
}

/**
 * Merge two objects, not checking for circularity, not merging arrays, modifying the first object
 * @param target {{}}
 * @param source {{}}
 * @param opts
 */
export function deepAssign (target: unknown, source: unknown, opts: DeepAssignOptions = {}) {
  if (typeof target !== "object" || target === null) {
    throw new TypeError("deepAssign must take in a (non-null) object")
  }

  let filledOpts = {
    cloneArrays: opts.cloneArrays ?? true,
    assignUndefined: opts.assignUndefined ?? false
  }

  return deepAssignInternal(target, source, filledOpts)
}

function deepAssignInternal (target: object, source: unknown, opts: DeepAssignFilledOptions) {
  if (typeof source !== 'object' || source === null)
    return source !== undefined || opts.assignUndefined ? source : target

  if (Array.isArray(target) || isTypedArray(target))
    return opts.cloneArrays ? deepClone(source) : source

  for (const key in source) {
    if (source.hasOwnProperty(key)) {
      let sourceVal = source[key]

      if (opts.assignUndefined || sourceVal !== undefined) {
        let val = target[key]
        let sourceIsArray = Array.isArray(sourceVal) || isTypedArray(sourceVal)

        if (typeof val === 'object' && !Array.isArray(val)) {
          if (typeof sourceVal === 'object' && !sourceIsArray) {
            deepAssign(val, sourceVal, opts)
            continue
          }
        }

        target[key] =
          (sourceIsArray && opts.cloneArrays) ? deepClone(sourceVal) : sourceVal
      }
    }
  }

  return target
}

/**
 * Same as deepAssign, but creating a copy of the target object.
 * @param target
 * @param source
 * @param opts
 */
export function deepMerge (target: unknown, source: unknown, opts : DeepAssignOptions) {
  if (typeof target !== "object" || target === null) return deepClone(source, opts)

  return deepAssign(deepClone(target, opts), source, opts)
}

/**
 * Deep clone an object, not checking for circularity or other weirdness, optionally cloning arrays
 * @param object
 * @param opts
 */
export function deepClone (object: unknown, opts: { cloneArrays?: boolean } = {}) {
  opts.cloneArrays = opts.cloneArrays ?? true

  return deepCloneInternal(object, opts.cloneArrays ?? true)
}

export type TypedArray = Uint8ClampedArray | Uint8Array | Uint16Array | Uint32Array | Int8Array | Int16Array | Int32Array | Float32Array | Float64Array
export type NumericArray = number[] | TypedArray

function deepCloneInternal (object: unknown, cloneArrays: boolean) {
  if (typeof object !== 'object') return object

  if (Array.isArray(object)) {
    return cloneArrays
      ? object.map(val => deepCloneInternal(val, cloneArrays))
      : object
  } else if (isTypedArray(object)) {
    // @ts-ignore
    return cloneArrays ? new object.constructor(object) : object  // object.constructor is okay for typedarrays
  }

  let ret = {}
  for (let key in object) {
    if (object.hasOwnProperty(key)) {
      ret[key] = deepCloneInternal(object[key], cloneArrays)
    }
  }

  return ret
}

export function isTypedArray (arr: unknown): boolean {
  return ArrayBuffer.isView(arr) && !(arr instanceof DataView)
}

export function isFloatArray (arr: unknown): boolean {
  let type = getTypedArrayType(arr)

  return type === "f32" || type === "f64"
}

export function getTypedArrayType (arr) {
  // TODO make all
  if (arr instanceof Float32Array) return "f32"
  if (arr instanceof Float64Array) return "f64"
}

export function getTypedArrayConstructor (type) {
  // TODO make all
  if (type === "f32") return Float32Array
  if (type === "f64") return Float64Array
}

/**
 * Arithmetic mod function instead of remainder
 * @param n {number}
 * @param m {number}
 * @returns {number}
 */
export function mod (n: number, m: number): number {
  let r = n % m
  return (r >= 0) ? r : (r + m)
}

/**
 * Freeze an object and all its children. Does not account for cycles
 * @param obj
 */
export function deepFreeze<T> (obj: T): T {
  Object.freeze(obj)

  if (typeof obj === "object" && obj !== null) {
    Object.values(obj).forEach(value => {
      if (typeof value === 'function' || typeof value === 'object')
        deepFreeze(value)
    })
  }

  return obj
}

// Remember left-pad?
export function leftZeroPad (str: string, len: number, char: string = '0') {
  if (str.length >= len) return str
  char = char[0] ?? '0'

  return char.repeat(len - str.length) + str
}

export function rightZeroPad (str: string, len: number, char: string = '0') {
  if (str.length >= len) return str
  char = char[0] ?? '0'

  return str + char.repeat(len - str.length)
}

export function trimLeft (str: string, char: string): string {
  char = char[0] ?? ' '

  let i = 0
  for (; i < str.length; ++i) {
    if (str.charAt(i) !== char)
      break
  }

  return str.substring(i)
}

/**
 * Credit to https://github.com/gustf/js-levenshtein/blob/master/index.js. Find the Levenshtein distance between two
 * strings.
 */
export const levenshtein = (function () {
  function _min (d0, d1, d2, bx, ay) {
    return d0 < d1 || d2 < d1
      ? d0 > d2
        ? d2 + 1
        : d0 + 1
      : bx === ay
        ? d1
        : d1 + 1
  }

  return function (a: string, b: string): number {
    if (a === b) {
      return 0
    }

    if (a.length > b.length) {
      let tmp = a
      a = b
      b = tmp
    }

    let la = a.length
    let lb = b.length

    while (la > 0 && a.charCodeAt(la - 1) === b.charCodeAt(lb - 1)) {
      la--
      lb--
    }

    let offset = 0

    while (offset < la && a.charCodeAt(offset) === b.charCodeAt(offset)) {
      offset++
    }

    la -= offset
    lb -= offset

    if (la === 0 || lb < 3) {
      return lb
    }

    let x = 0
    let y, d0, d1, d2, d3, dd, dy, ay, bx0, bx1, bx2, bx3

    let vector: Array<number | string> = []

    for (y = 0; y < la; y++) {
      vector.push(y + 1)
      vector.push(a.charCodeAt(offset + y))
    }

    let len = vector.length - 1

    for (; x < lb - 3; ) {
      bx0 = b.charCodeAt(offset + (d0 = x))
      bx1 = b.charCodeAt(offset + (d1 = x + 1))
      bx2 = b.charCodeAt(offset + (d2 = x + 2))
      bx3 = b.charCodeAt(offset + (d3 = x + 3))
      dd = x += 4
      for (y = 0; y < len; y += 2) {
        dy = vector[y]
        ay = vector[y + 1]
        d0 = _min(dy, d0, d1, bx0, ay)
        d1 = _min(d0, d1, d2, bx1, ay)
        d2 = _min(d1, d2, d3, bx2, ay)
        dd = _min(d2, d3, dd, bx3, ay)
        vector[y] = dd
        d3 = d2
        d2 = d1
        d1 = d0
        d0 = dy
      }
    }

    for (; x < lb; ) {
      bx0 = b.charCodeAt(offset + (d0 = x))
      dd = ++x
      for (y = 0; y < len; y += 2) {
        dy = vector[y]
        vector[y] = dd = _min(dy, d0, dd, bx0, vector[y + 1])
        d0 = dy
      }
    }

    return dd
  }
})()

const warnings = new Map<any, number>()

export function localWarn (s: string, id: any, maxCount: number=2) {
  let count = warnings.get(id)

  if (count as number >= maxCount) return  // undefined casts to 0

  if (!count) {
    count = 0
  }

  console.warn(`Warning ${id}: ${s}`)

  warnings.set(id, count + 1)
  if (count >= maxCount - 1) {
    console.warn(`Warning ${id} raised ${maxCount} times; no longer being reported`)
  }
}
