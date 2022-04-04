
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
    if (e instanceof Grapheme.BolusCancellationError) {
      // bolus suspended
    } else {
      throw e
    }
  })
}

function cancelDraw () {
  bolus?.cancel()
}

function * drawBolus () {
  let z = new Grapheme.Complex(0, 0)
  let ev = compiledExpression.targets[0].evaluate
  let results = new Float32Array(2 * dim * dim)
  let write = 0
  let pauseRow = 5

  for (let i = 0; i < dim; ++i) {
    z.im = (0.5 - i / dim) * zoom

    // Progress report
    if (i % pauseRow === 0) {
      yield i / dim
    }

    for (let j = 0; j < dim; ++j) {
      z.re = (j / dim - 0.5) * zoom

      let v = ev(z)

      results[write++] = v.re
      results[write++] = v.im
    }
  }

  new Grapheme.StandardColoringScheme({ type: "repeat_exponential" }).writeComplexArrayToRGBA(results, colors, 0)
  copyToDisplay()
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
let oversample = false

function setExpression (s) {
  if (exprInput.value !== s) exprInput.value = s

  try {
    let result = Grapheme.compileNode(`complex(${s})`, {
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
  //oversample = !!document.getElementById("oversample").checked

  draw()
}

function play() {
  setExpression("z^5-1")
  updateExpression()

  draw()
}
