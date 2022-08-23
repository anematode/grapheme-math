let l = 1000000, arr = new Int32Array([ ... new Array(l).keys()].map(() => Math.random() * (2 ** 31)))

let results = []

for (let i = 0; i < 1000; ++i) {
  let s = 0
  for (let i = 0; i < l; ++i) {
    let k = arr[i] | 0

    s = (s + k) | 0
  }

  results.push(s)
}

console.log(results)