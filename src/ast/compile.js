import {ASTNode} from "./node.js"

export class CompilationError extends Error {
  constructor (message) {
    super(message)

    this.name = 'CompilationError'
  }
}

/**
 * Compile an ASTNode into an evaluable function or functions. This should be preferred for any expression that will be
 * evaluated many times. This function should be pretty well optimized...
 * @param root
 * @param modes
 */
export function compileNode(root, {
  modes = ["normal"],
  exportedVariables = [],
  staticVariables = []
} = {}) {
  if (!(root instanceof ASTNode)) {
    throw new CompilationError("First argument to compileNode must be an ASTNode")
  }

  if (!root.allResolved()) {
    throw new CompilationError("Node types must be resolved with .resolveTypes() first")
  }

  // Deduced information about nodes
  let nodeInformation = new Map()
  root.applyAll(node => {
    nodeInformation.set(node, {})
  })

  return nodeInformation
}
