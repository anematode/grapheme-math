let canvas = document.createElement("canvas")
let dim = canvas.width = canvas.height = 750
document.getElementById("plot-container").appendChild(canvas)

let ctx = canvas.getContext('2d')
let colors = new Uint8ClampedArray(dim * dim * 4)

let ev

function play () {
  s = "exp(z)"
  ev = Grapheme.compileNode(s, {
    resolveTypes: {
      defaultType: "complex"
    }, targets: {
      inputFormat: [ 'z' ]
    } })
}

function draw () {
  let b = Grapheme.asyncDigest(drawBolus)
}

function * drawBolus () {
  let z = new Grapheme.Complex(0, 0)

  for (let i = 0; i < dim; ++i) {
    let a = (0.5 - i / dim) * 20
    z.im = a

    // Progress report
    if (i % 50 === 0) {
      yield i / dim
    }

    for (let j = 0; j < dim; ++j) {
      let b = (j / dim - 0.5) * 20
      z.re = b

      let v = ev.evaluate(z)

      Grapheme.writeComplexToRGBA(v, colors, 4 * (dim * i + j))
    }
  }

  ctx.putImageData(new ImageData(colors, dim, dim), 0, 0)
}
