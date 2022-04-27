import { BoundingBox } from "../../other/bounding_box";
import { roundUp } from "../../fp/manip";

// base-64 encoded
const WASM_CONTENTS = "$WASM_CONTENTS"

function toBuffer (bin: string) {
  let buffer = new Uint8Array(bin.length)
  for (let i = 0; i < bin.length; ++i) {
    let code = bin.charCodeAt(i)

    buffer[i] = code
  }

  return buffer
}

let F32: Float32Array, F64: Float64Array

window.chicken = ""
function log(o) {
  window.chicken += o + '\n'
}

const memory = new WebAssembly.Memory({ initial: 1, maximum: 10000 /* 64 kib pages */ })
const module = new WebAssembly.Module(toBuffer(atob(WASM_CONTENTS)))
const instance =
  new WebAssembly.Instance(module, { js: { mem: memory }, console: { log } })
wasmInstanceMinimumMemory(0)


/**
 * Returns false if it failed to allocate memory
 * @param bytes
 */
function wasmInstanceMinimumMemory(bytes: number) {
  bytes |= 0
  let grow = (bytes - memory.buffer.byteLength) | 0

  if (grow > 0) {
    memory.grow(1 + (grow >> 16))

    F32 = new Float32Array(memory.buffer)
    F64 = new Float64Array(memory.buffer)
  }
}

const bounding_box_flat_f32 = instance.exports.bounding_box_flat_f32 as Function;

window.c = instance.exports

function boundingBoxFlatF32 (arr: Float32Array): BoundingBox {
  wasmInstanceMinimumMemory(128 + arr.length << 2)

  // Copy into buffer. First 8 entries (32 bytes) are the found minima and maxima
  F32.set(arr, 8)
  F32.fill(100)
  bounding_box_flat_f32(32, arr.length)

  window.F32 = F32

  let xmin1 = F32[0]
  let ymin1 = F32[1]
  let xmin2 = F32[2]
  let ymin2 = F32[3]
  let xmax1 = F32[4]
  let ymax1 = F32[5]
  let xmax2 = F32[6]
  let ymax2 = F32[7]

  let xmin = xmin1 < xmin2 ? xmin1 : xmin2
  let ymin = ymin1 < ymin2 ? ymin1 : ymin2
  let xmax = xmax1 > xmax2 ? xmax1 : xmax2
  let ymax = ymax1 > ymax2 ? ymax1 : ymax2

  return new BoundingBox(xmin, ymin, roundUp(xmax - xmin), roundUp(ymax - ymin))
}

window.b = boundingBoxFlatF32
let add = 0

export { add }
