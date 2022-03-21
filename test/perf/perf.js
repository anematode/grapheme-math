(async () => {
  try {
    let GMath = await import("../../build/index.js")
    Object.assign(window, GMath)
  } catch (e) {
    console.log("Not local; importing from build")
    let GMath = await import("../../build/main.js")
    Object.assign(window, GMath)
  }

  if (typeof window.play === "function") {
    setTimeout(window.play, 0)
  }
})()

let overwriteLog = true

let inputBox = document.getElementById("console-input")
if (!inputBox) {
  console.log("In playground")
  overwriteLog = false
}

let consoleContents = document.getElementById("console-contents")
let inputHistory = []
let currentInput = ""

let historyIndex = 0

function pushToConsole(str) {
  consoleContents.value += str + '\n'
}

let oldLog = console.log
let oldError = console.error
let oldWarn = console.warn

export let log = (...args) => {
  pushToConsole("[LOG] " + args.join(' '))
  oldLog(...args)
}

export let error = (...args) => {
  pushToConsole("[ERROR] " + args.join(' '))
  oldError(...args)
}

export let warn = (...args) => {
  pushToConsole("[WARN] " + args.join(' '))
  oldWarn(...args)
}

if (overwriteLog)
  Object.assign(console, { log, error, warn })

function muckCommand (str) {
  // needed for the variable to be declared
  return str.replaceAll("let ", "var ")
}

function runConsoleCommand(str) {
  try {
    pushToConsole("> " + str)
    let value = (1, eval)(muckCommand(str))
    pushToConsole("< " + value)
    oldLog("Inputted: " + str)
  } catch (e) {
    pushToConsole("[ERROR] " + e + '')
    oldLog(e)
  } finally {
    inputHistory.push(str)
    historyIndex++
  }// runs in the global scope
}

function loadCommandToConsole() {
  let cmd = currentInput

  if (inputHistory.length !== 0 && historyIndex !== inputHistory.length) {
    cmd = inputHistory[historyIndex]
  }

  inputBox.value = cmd
}

if (inputBox) inputBox.onkeydown = evt => {
  if (evt.key === "Enter") {
    evt.preventDefault()
    let cmd = inputBox.value
    if (!cmd.trim()) {
      historyIndex = inputHistory.length
      return
    }

    inputBox.value = ""
    runConsoleCommand(cmd)

    historyIndex = history.length
  } else if (evt.key === "ArrowUp") {
    evt.preventDefault()

    if (history.length !== 0 && historyIndex > 0) {
      historyIndex--
      loadCommandToConsole()
    }
  } else if (evt.key === "ArrowDown") {
    evt.preventDefault()

    if (history.length !== 0 && historyIndex !== history.length) {
      historyIndex++
      loadCommandToConsole()
    }
  }
}

const DEFAULT_ITERATIONS = 100
// The final function is expected to be this much faster
const EXPECTED_WARMUP_IMPROVEMENT = 5

// ms, used to guess iterations
const DESIRED_BENCHMARK_TIME = 1000

function _benchmarkGeneric(f, iterations, inputs, exchangeLoops) {
  let start = performance.now()
  let inputCount = inputs.length

  if (inputs.length === 1) {
    let input = inputs[0]
    for (let i = 0; i < iterations; ++i) f(...input)
  } else if (exchangeLoops) {
    for (let j = 0; j < inputCount; ++j) for (let i = 0; i < iterations; ++i) f(...inputs[j])
  } else {
    for (let i = 0; i < iterations; ++i) for (let j = 0; j < inputCount; ++j) f(...inputs[j])
  }

  let end = performance.now()

  let totalInputs = iterations * inputCount
  let msPerInput = (end - start) / totalInputs
  return { f, iterations, exchangeLoops, inputs, totalInputs, msPerInput, total: end - start }
}

function _benchmarkNoInput(f, iterations, inputCount) {
  iterations *= inputCount

  let start = performance.now()
  for (let i = 0; i < iterations; ++i) f()
  let end = performance.now()

  let msPerInput = (end - start) / iterations

  return { f, iterations, exchangeLoops: false, inputs: [[]], totalInputs: iterations, msPerInput, total: end - start }
}

function _benchmarkSingleArg(f, iterations, inputs, exchangeLoops) {
  let flattenedInputs = inputs.map(a => a[0])

  let inputCount = inputs.length

  let start = performance.now()
  let result = 0
  if (exchangeLoops) {
    for (let j = 0; j < inputCount; ++j) for (let i = 0; i < iterations; ++i) result = f(flattenedInputs[j])
  } else {
    for (let i = 0; i < iterations; ++i) for (let j = 0; j < inputCount; ++j) result = f(flattenedInputs[j])
  }

  let end = performance.now()

  let totalInputs = iterations * inputCount
  let msPerInput = (end - start) / totalInputs
  return { f, iterations, exchangeLoops, inputs, totalInputs, msPerInput, total: end - start, result }
}

let TIME_PREC = 6
function showTime(ms) {
  if (ms < 1e-3) {
    return (ms * 1e6).toPrecision(TIME_PREC) + "ns"
  } else if (ms < 1) {
    return (ms * 1e3).toPrecision(TIME_PREC) + "µs"
  } else if (ms < 1000) {
    return ms.toPrecision(TIME_PREC) + "ms"
  } else {
    return (ms / 1000).toPrecision(TIME_PREC) + "s"
  }
}

/**
 *
 * @param f {Function}
 * @param opts
 */
export function benchmark(f, {
  iterations = -1, // -1 means guess iterations based on how long the warmup took
  doWarmup = true,
  warmupIterations = 100,
  inputs=[], // default is a single empty input; each input is an array
  exchangeLoops = false, // if true, run individual inputs multiple times in sequence; if false, run all inputs in sequence multiple times
  deoptimize = true // TODO
} = {}) {
  if (typeof f !== "function") throw new TypeError("f must be a function")

  iterations |= 0
  if (iterations < 0) doWarmup = true

  if (!Array.isArray(inputs)) return
  if (!inputs.length) inputs = [[]]
  let warmupTime = 0

  if (doWarmup) {
    warmupTime = performance.now()
    for (let i = 0; i < warmupIterations; ++i) {
      for (let input of inputs) {
        f(...input) // since this is a warmup, we're not going to be terribly precise
      }
    }
    warmupTime = performance.now() - warmupTime
  }

  warmupTime /= warmupIterations

  if (warmupTime === 0 && iterations < 0) {
    warn("Warmup was unmeasurable, using " + DEFAULT_ITERATIONS)
    iterations = DEFAULT_ITERATIONS
  } else if (iterations < 0) {
    iterations = Math.round(DESIRED_BENCHMARK_TIME * EXPECTED_WARMUP_IMPROVEMENT / warmupTime)
  }

  let overallArgCount = inputs[0].length
  let diffArgs = false
  // Check whether the types are all consistent, which may help
  let overallArgTypes = inputs[0].map(a => typeof a)
  let diffArgTypes = false
  let hasSingleInput = inputs.length === 1

  // We handle different input forms differently for tighter input loops
  for (let input of inputs) {
    let argCount = input.length
    if (argCount !== overallArgCount)
      diffArgs = true
    overallArgCount = Math.max(argCount, overallArgCount)

    if (!diffArgs && !diffArgTypes) {
      let argTypes = input.map(a => typeof a)
      for (let i = 0; i < argTypes.length; ++i) {
        if (argTypes[i] !== overallArgTypes[i]) {
          diffArgTypes = true
          break
        }
      }
    }
  }

  let res

  if (!diffArgs && overallArgCount === 0) {
    res = _benchmarkNoInput(f, iterations, inputs.length, exchangeLoops)
  } else if (!diffArgs && overallArgCount === 1) {
    res = _benchmarkSingleArg(f, iterations, inputs, exchangeLoops)
  } else {// TODO make others
    res = _benchmarkGeneric(f, iterations, inputs, exchangeLoops)
  }

  return {
    ...res,
    explain: () => `Benchmark ${res.name} completed ${res.totalInputs} inputs in ${showTime(res.total)}, average ${showTime(res.msPerInput)} per input`
  }
}

Object.assign(window, { benchmark })
