import { WebGLRenderer } from "./renderer.js";
import { getStringID, getVersionID } from "../utils";
import { Scene } from "./scene";
import { SceneDimensions } from "../other/scene_dims";
import { RendererContextInstruction, RenderingInfo, SceneContextInstruction } from "./renderer_instruction";


class AnnotatedSceneCopy {

}

class AnnotatedContextReducedCopy {

}

class ContextCompiledCopy {

}

// Corresponds one-to-one with an Element
class SceneAnnotatedNode {
  // Id of the element. If the id changes, everything under and including this element is invalidated (and is considered
  // a different element)
  id: string

  // Previous render info spat out by this instruction
  lastRenderingInfo: RenderingInfo

  // IDs of previous children of this node; we don't use the nodes themselves so that it's a flatter structure
  children: string[]
}

export class SceneGraph {
  id: string

  // Renderer this scene graph is attached to. Doesn't need to be attached to a renderer, in case static analysis is
  // desired
  renderer: WebGLRenderer | null

  sceneAnnotatedNodes: Map<string /* id */, SceneAnnotatedNode>
  sceneTopNode: null | string  // id of the scene node
  sceneNodeCount: number

  constructor (renderer: WebGLRenderer | null) {
    this.renderer = renderer
    this.id = getStringID()
    this.sceneAnnotatedNodes = new Map()

    this.sceneTopNode = null
    this.sceneNodeCount = -1
  }

  /**
   * Whether this scene graph has been built from a scene
   */
  hasScene (): boolean {
    return !!this.sceneTopNode
  }

  /**
   * Clear all caches, delete all buffer data
   */
  invalidateEverything () {

  }

  /**
   * Construct the scene graph from a scene by calling getRenderingInfo on all children, then performing an ordering
   * @param scene
   */
  buildFromScene (scene: Scene | null) {
    let sceneID = scene?.id

    if (sceneID !== this.sceneTopNode) { // The scene has changed, invalidate everything
      this.sceneTopNode = sceneID ?? null

      this.invalidateEverything()
    }
  }


  // Sort and shift around instructions based on their zIndices and escapeContext status
  orderInstructions () {

  }
}
