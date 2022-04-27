import { BoundingBox } from "../../other/bounding_box";
import { roundUp } from "../../fp/manip";

// base-64 encoded
const WASM_CONTENTS = "AGFzbQEAAAABCwJgAX8AYAJ/fwF/Ag8BB2NvbnNvbGUDbG9nAAADAgEBBQMBAGQHGQEVYm91bmRpbmdfYm94X2ZsYXRfZjMyAAEKSQFHAgF/A3sgACABaiECAkADQCAAIAAQAP0ABAAhBSADIAX96gEhAyAEIAX96wEhBCAAQSBqIQAgACACTgRADAIFDAELCwtBAAs="

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

function boundingBoxFlatF32 (arr: Float32Array): BoundingBox {
  // Copy into buffer
  wasmInstanceMinimumMemory(arr.length << 2)

  F32.set(arr)

  bounding_box_flat_f32(128, arr.length)

  let xmin = F32[0]
  let ymin = F32[1]
  let xmax = F32[2]
  let ymax = F32[3]

  return new BoundingBox(xmin, ymin, roundUp(xmax - xmin), roundUp(ymax - ymin))
}

window.b = boundingBoxFlatF32
let add = 0

export { add }
