
function calculateDomainColoring () {
  cancelDraw()
  colors.fill(255)

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
      //throw e
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

  new Grapheme.StandardColoringScheme({ type: coloringScheme, base: (coloringScheme === "normal") ? colorScale : 2 }).writeComplexArrayToRGBA(results, colors, 0)
}

function copyToDisplay () {
  ctx.putImageData(new ImageData(colors, dim, dim), 0, 0)
}

let canvas = document.createElement("canvas")
let dim = canvas.width = canvas.height = 750
document.getElementById("plot-container").appendChild(canvas)

let transform = new Grapheme.LinearPlot2DTransform()

transform.resizeToGraphBox({ cx: 0, cy: 0, w: 5, h: 5 })
transform.resizeToPixelBox({ x: 0, y: 0, w: dim, h: dim })

let ctx = canvas.getContext('2d')
let colors = new Uint8ClampedArray(dim * dim * 4)

let compiledExpression
let exprInput = document.getElementById("expr-input")
let bolus = null

let zoom = 1
let colorScale = 1
let coloringScheme = "normal"

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

  transform.resizeToGraphBox({ cx: 0, cy: 0, w: zoom, h: zoom })

  calculateDomainColoring()
}

let t = 0

function dotAt (v) {
  ctx.beginPath()
  ctx.arc(v.x, v.y, 8, 0, Math.PI*2, true)
  ctx.closePath()
  ctx.fill()
}

const Vec2 = Grapheme.Vec2
let radius = 1

function draw () {
  requestAnimationFrame(draw)
  copyToDisplay()

  let steps = 400

  let input = new Path2D()
  let output = new Path2D()
  let z = new Grapheme.Complex()
  let ev
  try {
    ev = compiledExpression.targets[0].evaluate
  } catch (e) {
    return
  }
  
  let isx, isy, sx, sy, after = () => {}

  for (let i = 0; i < steps; ++i) {
    z.expi(i / steps * 2 * Math.PI)
    z.multiplyReal(z, radius)

    let v = ev(z)

    let { x: vx, y: vy } = transform.graphToPixel(Vec2.fromObj(z))
    let { x, y } = transform.graphToPixel(Vec2.fromObj(v))

    if (i === t) {
      after = () => {
        ctx.fillStyle = "gray"
        dotAt(new Vec2(vx, vy))
        ctx.fillStyle = "yellow"
        dotAt(new Vec2(x, y))
        ctx.fillStyle = "black"
        dotAt(transform.graphToPixel(new Vec2(0, 0)))
      }
    }

    if (i === 0) {
      output.moveTo(sx = x, sy = y)
      input.moveTo(isx = vx, isy = vy)
    } else {
      output.lineTo(x, y)
      input.lineTo(vx, vy)
    }
  }

  t += 1
  t %= steps

  output.lineTo(sx, sy)
  input.lineTo(isx, isy)

  ctx.lineWidth = 3
  ctx.strokeStyle = "gray"
  ctx.stroke(input)
  ctx.strokeStyle = "black"
  ctx.stroke(output)

  after()
}

function play() {
  setExpression("z^5-1")
  updateExpression()

  draw()
}
