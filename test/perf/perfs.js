import { log, benchmark } from "./perf.js"
import {PATHOLOGICAL_NUMBERS, TYPICAL_NUMBERS} from "../test_common.js"
import { roundUp } from "../../src/main.js"
import { roundUp as referenceRoundUp } from "../../src/fp/reference.js"

let displayedPerfList = document.getElementById("test-list")

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

function runPerf(perf) {
  let results = benchmark(perf.benchmark.f, perf.benchmark)

  log(`Benchmark ${perf.name} completed ${results.totalInputs} inputs in ${showTime(results.total)}, average ${showTime(results.msPerInput)} per input`)
}

let perfList = [
  { type: "single", name: "roundUp", benchmark: { f: roundUp, inputs: TYPICAL_NUMBERS.map(a => [a]), iterations: 1e5, exchangeLoops: false } },
  { type: "single", name: "reference roundUp", benchmark: { f: referenceRoundUp, inputs: TYPICAL_NUMBERS.map(a => [a]), iterations: 4e4, exchangeLoops: false } }
]

for (let perf of perfList) {
  let elem = document.createElement("li")
  let label = document.createElement("p")

  label.classList.add("perf-name-label")
  label.innerText = perf.name

  let button = document.createElement("button")

  button.onclick = () => runPerf(perf)
  button.innerText = "Run"

  elem.appendChild(label)
  elem.appendChild(button)
  perf.elem = elem

  displayedPerfList.appendChild(elem)
}
