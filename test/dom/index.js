import * as Grapheme from "../../build/index.js"

if (!Grapheme.utils.inDOM) {
    throw new Error("Not in DOM")
}

class WrappedImage {
    constructor() {
        this.dom = new Element()

        this.dom.innerHTML = `<p class="wrapped-img-width">640px</p>
                    <div class="wrapped-img-lr">
                        <p class="wrapped-img-height">480px</p>
                        <div class="wrapped-img-container">

                        </div>
                    </div>`
        this.dom.classList.add("wrapped-img")
    }

    widthIndicator() {
        return this.dom.querySelector(".wrapped-img-width")
    }

    heightIndicator() {
        return this.dom.querySelector(".wrapped-img-height")
    }

    container() {
        return this.dom.querySelector(".wrapped-img-container")
    }

    resize(width, height) {
        this.widthIndicator().innerText = width + "px"
        this.heightIndicator().innerText = height + "px"

        let dpr = window.devicePixelRatio

        let p
    }
}

const testArena = document.getElementById("test-arena")

let arenaImg = new WrappedImage()
testArena.appendChild(arenaImg.dom)

let lastArenaRect = null

let MIN_SIZE

function arenaRect() {
    return lastArenaRect
}

function resizeTestArena(width, height) {
    width |= 0
    height |= 0

    testArena.parent.getE
}

resizeTestArena(640, 380)

function displayTestImage(canvas) {
    let child
    while ((child = testArena.lastChild)) {
        child.remove()
    }

    testArena.appendChild(canvas)

    resizeTestArena(canvas.width, canvas.height)
}

function displayTestScene(scene) {
    if (!(scene instanceof Grapheme.Scene)) throw "bad";
}

requestAnimationFrame(() => {
    lastArenaRect = testArena.getBoundingClientRect()

    console.log(lastArenaRect.x, lastArenaRect.y)
})
