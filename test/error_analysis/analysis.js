import * as fs from 'fs'
import {gammaReal} from "../../src/real/normal.js"

let testValueFolder = process.argv[2]
let testValueFile = testValueFolder + "/test_values.txt"

let values = fs.readFileSync(testValueFile).toString().split(',').filter(s => !!s).map(s => +s)
let testFunctions = {
  "gamma": gammaReal
}

let f = testFunctions[testValueFolder]

for (let i = 0; i < values.length; ++i) {
  console.log(f(values[i]))
}
