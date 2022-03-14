export class CompilationError extends Error {
  constructor (message) {
    super(message)

    this.name = 'CompilationError'
  }
}

/**
 * Compile an ASTNode into an evaluable function or functions. This should be preferred for any expression that will be
 * evaluated many times.
 * @param node
 * @param modes
 */
export function compileNode(node, {
  modes = ["normal"],
  exportedVariables = ["x", "y"],
  constants = {}
} = {}) {
  if (!node.allResolved()) {
    throw new CompilationError("Node types must be resolved with .resolveTypes() first")
  }
}
