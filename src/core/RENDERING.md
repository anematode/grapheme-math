## Rendering theory

Grapheme's renderer is fairly intricate. Elements propose how they want to be rendered with *render instructions*, defined by the types `RendererInstruction` and `RendererContextInstruction` (`renderer_instruction.ts`). These instructions are fairly primitive:

| Type | Parameters           | Notes   |
|------|----------------------|---------|
| primitive | `type`: `triangles`, `lines`, ... | corresponds directly to the underlying WebGL primitive |
| | `vertexData`: VertexData | Vertices to draw |
| | `color`: Color | Color with which to draw the primitive |

An element describes how it will be rendered with the result of `getRenderingInfo()`, which returns a `RenderingInfo` object, consisting of a list of `RendererContextInstruction`s and `RendererInstruction`s. A `RendererContextInstruction` is a distinct type of renderer instruction that describes a new *context*, a place in which further instructions have some special behavior. The most obvious example of a context instruction is a `SceneContextInstruction`, which describes the dimensions of a new scene. Less obvious examples include masking instructions and instructions that impose a rendering order.

Any instruction has several basic attributes, including a `version`, which, if provided, must be changed every time the instruction changes. (If not provided, the instruction is presumed to have changed every time.) The `RenderingInfo` object itself may also provide a `version`, which, if unchanged, means none of the underlying instructions have changed. These `version` objects are provided for optimization of the rendering—avoiding recomputation and lots of buffer data transfers to the GPU, which is invariably slow.

The elements' rendering info is collected in order (parents being evaluated first) and flattened according to the following algorithm:

... TODO explain ...

The instructions are then "compiled", meaning that they are not evaluated, but their corresponding vertex array objects are created (sent to the GPU), and, if they are complex instructions, they are converted into primitive instructions (e.g., polylines become triangle strips). This new type of instruction is called a `CompiledRendererInstruction`, and includes both context and more typical instructions.

A `Renderer` instance is essentially a giant state machine which blindly takes in `CompiledRendererInstruction`s and runs them on the canvas. The renderer itself contains versions, but does not *compare* them with old instructions and such; that is the job of the SceneGraph to emit `CompiledRendererInstruction`s in the correct order. The SceneGraph also has the job of rendering and loading text textures. Really, the `Renderer` is somewhat "dumb". It just holds the data in a consistent way.

### Scene Graph

So if the renderer is dumb, the scene graph has to pick up a lot of work. In particular, it has to do all of the following, in roughly decreasing order of difficulty:

- Optimize instructions when possible—for example, merging 200 consecutive instructions to draw circles into a single instruction with lots of triangles
- Perform text rendering and load the texture when necessary
- Gracefully recover when context loss is indicated (although the initial handling occurs in `Renderer`)
- Keep track of elements' instructions and their versions, comparing them when helpful
- Compute the correct order of instructions based on z index and escapeContext parameters

There are thus a few stages that a scene graph must undertake, in the following order:

- Call `getRenderingInfo()` on each child. That's simple enough.
- Create a node for each child, or, if the nodes already exist, update those nodes, keeping track of whether the scene structure has changed at all
- Check whether the instructions have changed for that child
- Combine instructions when possible, such as consecutive primitive instructions
- Compile uncompiled `RendererInstruction`s into `CompiledRendererInstruction`s, a process which includes loading vertex data into appropriate buffers and converting complex instructions into simple instructions
- Flatten the instructions into an instruction stream for the `Renderer`

Yikes. The scene graph thus actually has three parts to it. One, a tree that's essentially an annotated copy of the scene, with a single node for each child ("scene annotated copy"). Two, an annotated tree where the nodes are context nodes and the children of each node are either other context nodes or (uncompiled) renderer instructions ("context annotated copy"). Three, a tree where the nodes are *compiled* context nodes and the children are *compiled* renderer instructions ("context compiled copy"). There is a one-to-one correspondence between uncompiled and compiled context nodes, but not one between uncompiled and compiled instructions, due to the optimization part.
