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
import { localWarn } from "../utils.js";

type BolusReturn<T> = {
  value: number | T  // if done is false, a value between 0 and 1 may be returned. If done is true, the result is returned.
  done: boolean
}

type BolusResult<T> = {
  result: T
  timeElapsed: number
}

/**
 * A Bolus is any object with a next() function and potentially a cleanup() function. The cleanup() function is called
 * only if the bolus is terminated early. If the bolus is cancelled after completing digestion, cleanup() is not called.
 * next() returns { value: ..., done: false/true }. cleanup() is optional, and will be called if the generator finishes,
 * is canceled, or throws. value is a number between 0 and 1 representing the progress so far.
 */
export interface Bolus<T> {
  next: () => BolusReturn<T>
  cleanup?: () => void
}

export class BolusTimeoutError extends Error {
  constructor (message) {
    super(message)
    this.name = 'BolusTimeoutError'
  }
}

export class BolusCancellationError extends Error {
  constructor (message) {
    super(message)
    this.name = 'BolusCancellationError'
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
export function syncDigest<T> (bolus: T | Bolus<T>, timeout = -1): BolusResult<T> {
  // @ts-ignore
  if (bolus == null || typeof bolus?.next !== 'function') {
    // Forward non-boluses
    return { result: bolus as T, timeElapsed: 0 }
  }

  bolus = bolus as Bolus<T>
  const startTime = Date.now()

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
        const next = bolus.next()
        const delta = Date.now() - startTime

        if (next.done) {
          // return the result if done
          return { result: next.value as T, timeElapsed: delta }
        }

        if (delta > timeout) {
          // Clean up if needed
          bolus.cleanup?.()

          throw new BolusTimeoutError(
            'Bolus did not digest within ' + timeout + ' ms.'
          )
        }
      }
    } else if (timeout !== -1) {
      throw new RangeError(
        'Invalid timeout, which must be between 1 and 86,400,000 ms, or -1 to signify no timeout.'
      )
    }

    // timeout is -1
    while (true) {
      const next = bolus.next()

      if (next.done)
        return { result: next.value as T, timeElapsed: Date.now() - startTime }
    }
  } finally {
    // Potentially clean up
    bolus.cleanup?.()
  }
}

type BolusProgressCallback = (progress: number) => void

class BolusPromise<T> {
  _executor: Promise<BolusResult<T>>
  _bolus: Bolus<T> | null

  _startTime: number
  _timeout: number
  _timeStep: number

  _onProgress: BolusProgressCallback | null
  _onCleanup: Function | null
  _cancelled: boolean

  constructor (executor: Promise<BolusResult<T>> | null,
               bolus: Bolus<T> | null,
               st: number,
               timeout: number,
               timeStep: number,
               onProgress: BolusProgressCallback | null,
               onCleanup: Function | null,
               usePostMessage: boolean) {
    this._bolus = bolus
    this._startTime = st
    this._timeout = timeout
    this._timeStep = timeStep
    this._onProgress = onProgress
    this._onCleanup = onCleanup
    this._cancelled = false

    if (!executor) {
      this._executor = this.constructExecutor(usePostMessage)
    } else {
      this._executor = executor
    }
  }

  then (): Promise<any> {
    let e = this._executor
    return e.then.apply(e, arguments)
  }

  catch (): Promise<any> {
    let e = this._executor
    return e.catch.apply(e, arguments)
  }

  constructExecutor (usePostMessage: boolean): Promise<BolusResult<T>> {
    let id = usePostMessage ? Math.random() : 0
    let msgData = { asyncBolusId: id }  // posting this object will cause the corresponding bolus to step
    let tm

    let attachCallback = usePostMessage ? () => {
      window.addEventListener("message", tm)
      window.postMessage(msgData, location.origin)
    } : () => {
      id = setTimeout(tm, 0)
    }

    let scheduleCallback = usePostMessage ? () => {
      window.postMessage(msgData, tm)
    } : () => id = setTimeout(tm, 0)

    let cleanupCallback = usePostMessage ? () => {
      window.removeEventListener("message", tm)
    } : () => clearTimeout(id)

    return new Promise<BolusResult<T>>((resolve, reject) => {
      tm = (e: MessageEvent) => {
        if (e.data?.asyncBolusId !== id) return

        try {
          if (this._cancelled) {
            throw new BolusCancellationError("Bolus was cancelled")
          }

          let stepResult = this._step()

          if (stepResult.done) {
            cleanupCallback()

            resolve({ result: stepResult.value as T, timeElapsed: Date.now() - this._startTime })
            this._onProgress?.(1)

            return
          } else {
            this._onProgress?.(stepResult.value as number)
          }
          // timeouts/errors will call reject implicitly

          // Post message to itself
          scheduleCallback()
        } catch (e) {
          this._onCleanup?.()
          cleanupCallback()

          reject(e)
        }
      }

      attachCallback()
    })
  }

  _step (): BolusReturn<T> {
    let begin = Date.now()
    let startTime = this._startTime
    let step = this._timeStep
    let bolus = this._bolus!
    let timeout = this._timeout

    while (true) {
      const next = bolus.next()

      if (next.done) {
        // return the result if done
        return next
      }

      const delta = Date.now() - startTime

      if (timeout !== -1 && delta > timeout) {
        bolus.cleanup?.()
        throw new BolusTimeoutError(
          'Bolus did not digest within ' + timeout + ' ms.'
        )
      }

      const stepDelta = Date.now() - begin

      if (stepDelta > step) {
        return next
      }
    }
  }

  cancel () {
    this._cancelled = true
    return this
  }
}

type AsyncDigestOptions = {
  // milliseconds
  timeout?: number
  timeStep?: number
  onProgress?: BolusProgressCallback
  cleanup?: () => void
  usePostMessage?: boolean
}

/**
 * Digest a bolus asynchronously.
 * @param bolus
 * @param opts
 */
export function asyncDigest<T> (bolus: Bolus<T>, opts: AsyncDigestOptions = {}): BolusPromise<T> {
  if (typeof bolus?.next !== 'function') {
    localWarn("Nonbolus passed to asyncDigest (.next is either not a property or not a function", "asyncDigest nonbolus", 1)

    // Forward non-boluses
    return new BolusPromise<T>(Promise.resolve({ result: bolus as unknown as T, timeElapsed: 0 }),
      null, 0, 0, 0, null, null, false)
  }

  let startTime = Date.now()

  let timeout = opts.timeout ?? -1
  let timeStep = opts.timeStep ?? 10 // ms
  let onProgress = opts.onProgress ?? null
  let onCleanup = opts.cleanup ?? null
  let usePostMessage = opts.usePostMessage ?? true

  return new BolusPromise<T>(null, bolus, startTime, timeout, timeStep, onProgress, onCleanup, usePostMessage)
}
