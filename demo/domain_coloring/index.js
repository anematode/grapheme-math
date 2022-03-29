
function draw () {
  cancelDraw()

  bolus = Grapheme.asyncDigest(drawBolus(), {
    onProgress: p => document.getElementById("render-progress").innerText = (100 * p).toFixed(1) + '%'
  })

  bolus.then(({ timeElapsed }) => {
    // done
    bolus = null
    copyToDisplay()

    document.getElementById("render-time").innerText = timeElapsed.toFixed(1) + 'ms'
  }).catch(e => {
    // bolus suspended
  })
}

function cancelDraw () {
  bolus?.cancel()
}

function * drawBolus () {
  let z = new Grapheme.Complex(0, 0)
  let ev = compiledExpression.targets[0].evaluate

  for (let i = 0; i < dim; ++i) {
    let a = (0.5 - i / dim) * zoom
    z.im = a

    // Progress report
    if (i % 5 === 0) {
      yield i / dim
    }

    for (let j = 0; j < dim; ++j) {
      let b = (j / dim - 0.5) * zoom
      z.re = b

      let v = ev(z)

      Grapheme.writeComplexToRGBA(v, colors, 4 * (dim * i + j), 1 / colorScale)
    }
  }
}

function copyToDisplay () {
  ctx.putImageData(new ImageData(colors, dim, dim), 0, 0)
}

function copyWhileWorking () {
  if (bolus)
    copyToDisplay()

  requestAnimationFrame(copyWhileWorking)
}

requestAnimationFrame(copyWhileWorking)

let canvas = document.createElement("canvas")
let dim = canvas.width = canvas.height = 750
document.getElementById("plot-container").appendChild(canvas)

let ctx = canvas.getContext('2d')
let colors = new Uint8ClampedArray(dim * dim * 4)

let compiledExpression
let exprInput = document.getElementById("expr-input")
let bolus = null

let zoom = 1
let colorScale = 1

function setExpression (s) {
  exprInput.value = s

  try {
    let result = Grapheme.compileNode(s, {
      resolveTypes: {
        defaultType: "complex"
      }, targets: {
        inputFormat: ['z']
      }
    })

    compiledExpression = result
    setCodeOutput("Compiled expression " + s, false)
  } catch (e) {
    compiledExpression = null
    setCodeOutput(errToString(e), true)
  }
}

function updateExpression () {
  setExpression(exprInput.value)

  zoom = +document.getElementById("zoom-input").value
  colorScale = Math.sqrt(+document.getElementById("color-input").value) / 2

  draw()
}

function play() {
  setExpression("z^5-1")
  updateExpression()

  draw()
}
