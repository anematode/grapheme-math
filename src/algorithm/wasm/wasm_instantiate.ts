import { BoundingBox } from "../../other/bounding_box";
import { roundUp } from "../../fp/manip";

// base-64 encoded
const WASM_CONTENTS = "AGFzbQEAAAABEgRgAX8AYAF9AGABfABgAn9/AAIrAwdjb25zb2xlA2xvZwAAB2NvbnNvbGUDbG9nAAEHY29uc29sZQNsb2cAAgMCAQMFBAEAkE4HIgIVYm91bmRpbmdfYm94X2ZsYXRfZjMyAAMGbWVtb3J5AgAKkwEBkAECAX8De/0MAACAfwAAgH8AAIB/AACAfyED/QwAAID/AACA/wAAgP8AAID/IQQgACABaiECAkADQCADIAD9AAQAIgX96gEhAyAEIAX96wEhBCADIABBEGr9AAQAIgX96gEhAyAEIAX96wEhBCAAQSBqIgAgAk4EQAwCBQwBCwsLQQAgA/0LBABBECAE/QsEAAs="

function toBuffer (bin: string) {
  let buffer = new Uint8Array(bin.length)
  for (let i = 0; i < bin.length; ++i) {
    let code = bin.charCodeAt(i)

    buffer[i] = code
  }

  return buffer
}

window.chicken = ""
function log(o) {
  window.chicken += o + '\n'
}

const module = new WebAssembly.Module(toBuffer(atob(WASM_CONTENTS)))
const instance =
  new WebAssembly.Instance(module, { js: {
    mem: new WebAssembly.Memory({ initial: 0, maximum: 10000 /* 64 kib pages */ })
    }, console: { log } })

let memory: WebAssembly.Memory = instance.exports.memory as WebAssembly.Memory

const HEAP_F32 = new Float32Array(memory.buffer)
const HEAP_F64 = new Float64Array(memory.buffer)
const HEAP_I32 = new Int32Array(memory.buffer)
const HEAP_U32 = new Uint32Array(memory.buffer)

const bounding_box_flat_f32 = instance.exports.bounding_box_flat_f32 as Function;


function boundingBoxFlatF32 (arr: Float32Array): BoundingBox | null {
  // Copy into buffer. First 8 entries (32 bytes) are the found minima and maxima
  HEAP_F32.set(arr, 8)

  // Fill extra space with NaNs because the internal implementation doesn't take care of the edges
  let len = arr.length | 0, endFill = (15 + len) & (~0b11)
  for (let i = 8 + len; i < endFill; ++i) {
    HEAP_F32[i] = NaN
  }

  bounding_box_flat_f32(32, arr.length << 2)

  let xmin1 = HEAP_F32[0]
  let ymin1 = HEAP_F32[1]
  let xmin2 = HEAP_F32[2]
  let ymin2 = HEAP_F32[3]
  let xmax1 = HEAP_F32[4]
  let ymax1 = HEAP_F32[5]
  let xmax2 = HEAP_F32[6]
  let ymax2 = HEAP_F32[7]

  let xmin = xmin1 < xmin2 ? xmin1 : xmin2
  let ymin = ymin1 < ymin2 ? ymin1 : ymin2
  let xmax = xmax1 > xmax2 ? xmax1 : xmax2
  let ymax = ymax1 > ymax2 ? ymax1 : ymax2

  if (xmin === Infinity || ymin === Infinity) { // no valid entries found
    return null
  }

  return new BoundingBox(xmin, ymin, roundUp(xmax - xmin), roundUp(ymax - ymin))
}

const WASM = {
  HEAP_F32,
  HEAP_F64,
  HEAP_I32,
  HEAP_U32,

  instance,
  exports: instance.exports
}

export { WASM, boundingBoxFlatF32 }
