import { WebGLRenderer } from "./renderer.js";
import { getStringID, getVersionID } from "../utils";
import { Scene } from "./scene";
import { SceneDimensions } from "../other/scene_dims";
import { RendererContextInstruction, RenderingInfo, SceneContextInstruction } from "./renderer_instruction";

type BaseSceneGraphContextNode = {
  id: string
  // Order to draw in
  zIndex: number
  // Depth from top
  contextDepth: number
  // Context which this context is a member of
  parent: null | SceneGraphContextNode
}

type SceneGraphSceneContextNode = BaseSceneGraphContextNode & {
  type: "scene"
  dims: SceneDimensions
  // Only context nodes may have children--everything else is flattened
  children: BaseSceneGraphContextNode[]
  parent: null
}

type SceneGraphContextNode = SceneGraphSceneContextNode

export class SceneGraph {
  id: string

  // Renderer this scene graph is attached to. Doesn't need to be attached to a renderer, in case static analysis is
  // desired
  renderer: WebGLRenderer | null

  // Map id -> context node
  contexts: Map<string, SceneGraphContextNode>

  constructor (renderer: WebGLRenderer | null) {
    this.renderer = renderer
    this.id = getStringID()
  }

  _clearAll () {
    this.contexts.clear()
  }

  /**
   * Construct the scene graph from a scene by calling getRenderingInfo on all children, then performing an ordering
   * @param scene
   */
  buildFromScene (scene: Scene) {
    this._clearAll()

    let sceneInfo = scene.getRenderingInfo()
    if (!sceneInfo || !sceneInfo.contexts || sceneInfo.contexts.length === 0) throw new Error("Scene has no contexts")

    let firstSceneContext = sceneInfo.contexts[0]
    if (firstSceneContext.insnType !== "scene") throw new Error("Scene's first context instruction is not a scene instruction")

    let topGraphContext: SceneGraphSceneContextNode = {
      id: "top",
      type: "scene",
      dims: firstSceneContext.dims,
      zIndex: -1, // z index doesn't particularly matter here
      contextDepth: 0,
      children: [],
      parent: null
    }

    let contextMap = this.contexts
    contextMap.set("top", topGraphContext)

    let currentContext: null | SceneGraphContextNode = topGraphContext

    function popContext() {
      if (!currentContext)
        throw new Error("Tried to pop too many contexts")
      currentContext = currentContext.parent
    }

    scene.apply(e => {
      let renderingInfo = e.getRenderingInfo()

      console.log(renderingInfo)
    })
  }

  // Sort and shift around instructions based on their zIndices and escapeContext status
  orderInstructions () {

  }
}
