import { SceneGraph } from "../build/core/scene_graph.js"
import { Scene } from "../build/core/scene.js"
import { expect } from "chai"

describe("SceneGraph", () => {
  let renderer = null

  function createDummyScene () {
    let scene = new Scene()

    return scene
  }

  describe("hasScene", () => {
    let scene = createDummyScene()

    // If there's no renderer, the graph is simply detached, which is fine for, say, analysis
    let graph = new SceneGraph(renderer)

    expect(graph.hasScene()).to.eq(false)
    graph.buildFromScene(scene)
    expect(graph.hasScene()).to.eq(true)

    graph.buildFromScene(null)
    expect(graph.hasScene()).to.eq(false)
  })
})
