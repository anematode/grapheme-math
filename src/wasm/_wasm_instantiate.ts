
// base-64 encoded
const WASM_CONTENTS = "$WASM_CONTENTS"

const module = new WebAssembly.Module(new TextEncoder().encode(atob(WASM_CONTENTS)))
const instance =
  new WebAssembly.Instance(module, {});

const { add } = instance.exports;
for (let i = 0; i < 10; i++) {
  console.log(add(i, i));
}

export { add }
