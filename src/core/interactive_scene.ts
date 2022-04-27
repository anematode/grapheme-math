/**
 * A scene endowed with an actual DOM element.
 */
import { Scene } from "./scene.js";
import { ElementInternalStore } from "./element.js";
import { Vec2 } from "../vec/vec2.js";
import { calculateRectShift } from "../other/text_utils.js";
import { BoundingBox } from "../other/bounding_box.js";

type InteractiveSceneInternal = ElementInternalStore & {
  interactivityListeners: null | {
    [key: string]: (evt: MouseEvent) => void
  },
  htmlElements: SceneHTMLBauble[]
}

type SceneHTMLBauble = {
  content: string
  type: "html" | "latex"
  pos: Vec2, domElement: HTMLDivElement, w: number, h: number, claimed: boolean
}

export class InteractiveScene extends Scene {
  domElement: HTMLDivElement
  domCanvas: HTMLCanvasElement
  bitmapRenderer: ImageBitmapRenderingContext

  internal: InteractiveSceneInternal

  init (params) {
    super.init(params)

    this.domElement = document.createElement("div")
    this.domElement.style.position = "relative" // so that absolute html children are positioned relative to the div

    this.domCanvas = document.createElement('canvas')

    this.domCanvas.id = this.id

    this.domElement.appendChild(this.domCanvas)
    this.bitmapRenderer = this.domCanvas.getContext('bitmaprenderer') ?? throwNoBitmapRenderer()
  }

  _disableInteractivityListeners () {
    let internal = this.internal
    let interactivityListeners = internal.interactivityListeners

    if (!interactivityListeners) return

    for (let listenerType in interactivityListeners) {
      let listener = interactivityListeners[listenerType]

      this.domElement.removeEventListener(listenerType, listener)
    }

    internal.interactivityListeners = null
  }

  _enableInteractivityListeners () {
    this._disableInteractivityListeners()
    let listeners = (this.internal.interactivityListeners = {})

    // Convert mouse event coords (which are relative to the top left corner of the page) to canvas coords
    const getSceneCoords = evt => {
      let rect = this.domElement.getBoundingClientRect()
      return new Vec2(evt.clientX - rect.x, evt.clientY - rect.y)
    }

    ;['mousedown', 'mousemove', 'mouseup', 'wheel'].forEach(eventName => {
      let listener

      if (eventName === 'wheel') {
        listener = evt => {
          this.triggerEvent(eventName, {
            pos: getSceneCoords(evt),
            deltaY: evt.deltaY
          })
          evt.preventDefault()
        }
      } else {
        listener = evt => {
          this.triggerEvent(eventName, { pos: getSceneCoords(evt) })
          evt.preventDefault()
        }
      }

      let elem = eventName === "mouseup" ? document : this.domElement
      elem.addEventListener(
        eventName,
        (listeners[eventName] = listener)
      )
    })
  }

  setInteractivity (enable: boolean) {
    this.props.set("interactivity", enable, 0, 1)
    this._updateInteractivity()
  }

  _updateInteractivity () {
    let enable = this.props.get("interactivity")
    let internal = this.internal
    if (!!internal.interactivityListeners !== enable) {
      enable
        ? this._enableInteractivityListeners()
        : this._disableInteractivityListeners()
    }
  }

  _update () {
    super._update()

    this._updateInteractivity()
    this.resizeCanvas()
  }

  resizeCanvas () {
    if (this.props.hasChanged("sceneDims")) {
      let sceneDims = this._getDims()
      let c = this.domCanvas

      c.width = sceneDims.canvasWidth
      c.height = sceneDims.canvasHeight

      c.style.width = sceneDims.width + 'px'
      c.style.height = sceneDims.height + 'px'
    }
  }

  addHTMLElement (element) {
    let domElement = this.domElement

    domElement.appendChild(element)
  }

  setHTMLElements (instructions) {
    let internal = this.internal, htmlElements
    let that = this // huzzah

    if (!internal.htmlElements)
      internal.htmlElements = []

    htmlElements = internal.htmlElements

    htmlElements.forEach(elem => elem.claimed = false)

    function addElementToDOM (html) {
      let div = document.createElement("div")
      div.innerHTML = html

      div.style.position = "absolute"
      div.style.left = div.style.top = '0'
      div.style.visibility = "none"

      that.domElement.appendChild(div)

      let rect = div.getBoundingClientRect()
      return { div, rect }
    }

    function addElement (html, pos, dir, spacing, transform): SceneHTMLBauble {
      let { div, rect } = addElementToDOM(html)

      let shiftedRect = calculateRectShift(new BoundingBox(pos.x, pos.y, rect.width, rect.height), dir, spacing)

      div.style.left = shiftedRect.x + 'px'
      div.style.top = shiftedRect.y + 'px'
      div.style.transform = transform

      return {
        type: "html", pos: new Vec2(shiftedRect.x, shiftedRect.y), domElement: div,
        w: rect.width, h: rect.height, claimed: true, content: ""
      }
    }

    main: for (const instruction of instructions) {
      if (instruction.type === "latex") {
        let { pos, dir, spacing, scale } = instruction

        for (const elem of htmlElements) {
          if (elem.claimed || elem.type !== "latex" || elem.content !== instruction.content) continue

          // then the element's latex content is the same, so we calculate the new position. Note we reuse the old
          // width/height values so that getBoundingClientRect() is only called once
          let shiftedRect = calculateRectShift(new BoundingBox(pos.x, pos.y, elem.w, elem.h), dir, spacing)

          pos = shiftedRect.tl()
          if (elem.pos.x !== pos.x || elem.pos.y !== pos.y) { // need to move the element
            elem.domElement.style.left = shiftedRect.x + 'px'
            elem.domElement.style.top = shiftedRect.y + 'px'

            elem.pos = pos
          }

          elem.claimed = true
          continue main
        }

        // No latex element exists that's unclaimed and has the same content, so we create one
        let elem = addElement(instruction.html, pos, dir, spacing, `matrix(${scale}, 0, 0, ${scale}, 0, 0)`)

        elem.type = "latex"
        elem.content = instruction.content

        htmlElements.push(elem)
      } else {
        // ignore for now
      }
    }

    // Destroy unclaimed html elements
    this.internal.htmlElements = htmlElements.filter(elem => {
      let claimed = elem.claimed

      if (!claimed) {
        this.domElement.removeChild(elem.domElement)
      }

      return claimed
    })
  }

  destroyHTMLElements () {
    let children = Array.from(this.domElement.children) as HTMLElement[]

    for (const child of children) {
      if (child.id !== this.id) {
        child.style.visibility = "none"

        this.domElement.removeChild(child)
      }
    }
  }
}

function throwNoBitmapRenderer(): never {
  throw new Error("Browser does not support bitmap renderer")
}
