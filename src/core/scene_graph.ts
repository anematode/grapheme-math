import { WebGLRenderer } from "./renderer.js";
import { getStringID } from "../utils";
import { Scene } from "./scene";
import { SceneDimensions } from "../other/scene_dims";
import { RendererContextInstruction, RenderingInfo, SceneContextInstruction } from "./renderer_instruction";

type BaseSceneGraphContextNode = {

}

type SceneGraphSceneContextNode = BaseSceneGraphContextNode & {
  type: "scene"
  dims: SceneDimensions
  id: string
}

type SceneGraphContextNode = SceneGraphSceneContextNode

export class SceneGraph {
  id: string
  // Renderer this scene graph is attached to. Doesn't need to be attached to a renderer, in case static analysis is
  // desired
  renderer: WebGLRenderer | null

  contexts: Map<string, SceneGraphContextNode>

  constructor (renderer: WebGLRenderer | null) {
    this.renderer = renderer
    this.id = getStringID()
  }

  _clearAll () {
    this.contexts.clear()
  }

  _createEmptyContext (insn: RendererContextInstruction, name?: string) {
    let context

    if (!name) name = getStringID()

    switch (insn.insnType) {
      case "scene":
        context = {
          type: "scene",
          dims: insn.dims,
          id: name
        }
      default:
        throw new Error(`Unknown context instruction type: ${insn.insnType}`)
    }
  }

  /**
   * Construct the scene graph from a scene by calling getRenderingInfo on all children
   * @param scene
   */
  buildFromScene (scene: Scene) {
    this._clearAll()

    let contexts = this.contexts
    let sceneInfo = scene.getRenderingInfo()

    // The first context for a scene MUST be a scene instruction

    if (!sceneInfo || sceneInfo.contexts?.length == 0) throw new Error("Top scene rendering info has no contexts")
    // Topmost context
    let topInstruction = sceneInfo.contexts![0] as SceneContextInstruction

    if (topInstruction.insnType !== "scene") {
      throw new Error("First context must be a scene context")
    }

    let top
  }
}
