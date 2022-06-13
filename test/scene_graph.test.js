import { SceneGraph } from "../build/core/scene_graph.js"
import { Scene } from "../build/core/scene.js"
import { expect } from "chai"
import {PolylineElement} from "../build/elements/polyline_element.js"
import {Group} from "../build/core/group.js"

describe("SceneGraph", () => {
  let renderer = null

  function createDummyScene () {
    let scene = new Scene()
    scene.id = "scene"

    let elem1 = new PolylineElement()
    elem1.id = "elem1"
    let elem2 = new PolylineElement()
    elem2.id = "elem2"
    let elem3 = new PolylineElement()
    elem3.id = "elem3"

    let grp1 = new Group()
    grp1.id = "grp1"

    scene.add(elem1).add(elem2).add(grp1.add(elem3))

    return scene
  }

  function createGraph () {
    let scene = createDummyScene()
    let graph = new SceneGraph(renderer)

    return [ scene, graph ]
  }

  it("hasScene", () => {
    let scene = createDummyScene()

    // If there's no renderer, the graph is simply detached, which is fine for, say, analysis
    let graph = new SceneGraph(renderer)

    expect(graph.hasScene()).to.eq(false)
    graph.buildFromScene(scene)
    expect(graph.hasScene()).to.eq(true)

    graph.buildFromScene(null)
    expect(graph.hasScene()).to.eq(false)
  })

  it("correctly counts nodes and gets node order", () => {
    let [ scene, graph ] = createGraph()

    expect(graph.sceneNodeCount).to.eq(-1)
    graph.buildFromScene(scene)
    expect(graph.sceneNodeCount).to.eq(5)
    expect(graph._flattenedAnnotatedGraphIDs()).to.deep.eq([
      "scene", "elem1", "elem2", "grp1", "elem3"
    ])

    // Reoder elem1 and elem2
    scene.children[1].setOrder(10)
    scene.sortChildren()

    graph.buildFromScene(scene)
    expect(graph.sceneNodeCount).to.eq(5)
    expect(graph._flattenedAnnotatedGraphIDs()).to.deep.eq([
      "scene", "elem1", "grp1", "elem3", "elem2"
    ])

    graph.buildFromScene(null)
    expect(graph.sceneNodeCount).to.eq(-1)
  })

  it("gets correct node order", () => {

  })
})
