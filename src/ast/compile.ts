
// @ts-nocheck
import {ASTNode, EvaluationError} from "./node.js"
import {toEvaluationMode} from "./eval_modes.js"


export class CompilationError extends Error {
  constructor (message) {
    super(message)

    this.name = 'CompilationError'
  }
}

/**
 * An assignment graph is not exactly a graph... but whatever. It is a series of assignments, potentially with branches
 * (not yet implemented).
 *
 * Each element in the list of assignments defines exactly one variable. For example { type: "assignment", name: "$2",
 * operatorDefinition: OperatorDefinition, mathematicalType: MathematicalType, args: ["x", "$1"] } is an assignment
 */

class AssignmentGraphNode {
  constructor (parentGraph) {
    this.parentGraph = parentGraph

    this.mathematicalType = null
    this.concreteType = null

    this.operatorDefinition = null
    this.evaluator = null
    this.nodeType = "constant"

    this.children = []
    this.associatedASTNode = null
  }

  /**
   * Apply a function to this node and all of its children, recursively.
   * @param func {Function} The callback function. We call it each time with (node, depth) as arguments
   * @param childrenFirst {boolean} Whether to call the callback function for each child first, or for the parent first.
   */
  applyAll (func, childrenFirst = false) {
    if (!childrenFirst) func(this)

    let children = this.children
    for (let i = 0; i < children.length; ++i) {
      let child = children[i]
      child.applyAll(func, childrenFirst)
    }

    if (childrenFirst) func(this)

    return this
  }

  /**
   * Get children's mathematical types as an array
   * @returns {MathematicalType[]}
   */
  getChildMathematicalTypes () {
    return this.children.map(c => c.mathematicalType)
  }

  /**
   * Get children's concrete types as an array
   * @returns {ConcreteType[]}
   */
  getChildConcreteTypes () {
    return this.children.map(c => c.concreteType)
  }

  buildChildrenFromASTNode (depth) {
    if (depth > 500) // prevent infinite loop
      throw new CompilationError(`Maximum node depth exceeded`)

    let children = this.children = []
    let associatedASTNode = this.associatedASTNode

    if (!associatedASTNode) {
      throw new CompilationError("?")
    }

    // Construct assignment nodes
    let astChildren = associatedASTNode.children

    if (astChildren)
    for (let i = 0; i < astChildren.length; ++i) {
      let astChild = astChildren[i]
      let astCast = associatedASTNode.casts[i]

      let fail = 0

      // Descend down plain groups (which should only have one child each)
      while (astChild.nodeType() === ASTNode.TYPES.ASTGroup) {
        astChild = astChild.children[0]

        if (!astChild)
          throw new CompilationError(`ASTGroup contains no child??`)
        if (fail++ > 500) // prevent infinite loop
          throw new CompilationError(`Maximum node depth exceeded`)
      }


      // Reached a non-trivial node
      let child = new AssignmentGraphNode(this.parentGraph)
      let attachTo = this

      if (!astCast.isIdentity()) {
        attachTo = new AssignmentGraphNode(this.parentGraph)

        attachTo.associatedASTNode = astChild
        attachTo.mathematicalType = astCast.type
        attachTo.nodeType = "operator"

        children[i] = attachTo
      }

      let astType
      switch (astType = astChild.nodeTypeAsString()) {
        case "ConstantNode":
          child.nodeType = "constant"
          break
        case "VariableNode":
          child.nodeType = "variable"
          break
        case "OperatorNode":
          child.nodeType = "operator"
          child.operatorDefinition = astChild.operatorDefinition
          break
        case "ASTGroup":
          break
        default:
          throw new CompilationError(`Unknown ASTChild type ${astType}`)
      }

      child.associatedASTNode = astChild
      child.mathematicalType = astChild.type

      attachTo.children.push(child)
    }

    for (let i = 0; i < children.length; ++i) {
      children[i].buildChildrenFromASTNode(depth+1, variableLocations)
    }
  }
}

/**
 * Contains entirely assignment graph nodes. Assignments are not necessarily JS assignments per se,
 * but they occupy a much, much lower level of graph than the original AST. All casts are converted into assignments, so
 * that each assignment takes a concrete type to the same concrete type. Each assignment graph node is associated with
 * its original AST node.
 *
 * At first, each assignment is an OperatorDefinition or constant. Identity casts are elided immediately for efficiency.
 * Then concrete types and concrete evaluators are established. Finally, code fragments are generated and assembled into
 * a final closure, to be invoked.
 */
class AssignmentGraph {
  constructor () {
    this.root = null
  }
}

function generateAssignmentGraph(astRoot, opts) {
  let g = new AssignmentGraph()
  let root = new AssignmentGraphNode(g)

  let variables = g.variables = new Map()   // varInfo -> AssignmentGraphNode

  for (let [ varName, varInfo ] of maps) {
    let assnNode = new AssignmentGraphNode(g)
    assnNode.nodeType = "variable"
    assnNode.
    variables.set(varName, )
  }

}

function analyzeNode (root, infoMap) {
  root.applyAll(node => {
    // For each
  })
}

function logVariableLocations(variableLocations, log) {
  for (let [ varName, varInfo ] of variableLocations.entries()) {
    let location = varInfo.location

    switch (location) {
      case "evaluate":
        log(() => `Variable ${varName} is to be evaluated (${varInfo.constant ? "constant" : "nonconstant"})`)
        break
      case "static":
        log(() => `Variable ${varName} is static`)
        break
      case "scope":
        log(() => `Variable ${varName} is to be found in the scope object`)
        break
      case "input":
        log(() => `Variable ${varName} is to be found at index ${varInfo.index}`)
        break
    }
  }
}

function getVariableLocations(staticVariables, usedVariables, inputFormat, log, doDebug) {
  /**
   * Map between variables and where to find them
   * @type {Map<string, { location: string }>}
   */
  let variableLocations = new Map()

  // Whether a scope is used
  let scopeIndex = inputFormat.indexOf('scope')
  let usesScope = scopeIndex !== -1

  if (usesScope) {
    let dup = inputFormat.indexOf('scope', scopeIndex + 1)
    if (dup !== -1) {
      throw new CompilationError(`Scope is defined twice, at indices ${scopeIndex} and ${dup}`)
    }

    variableLocations.set("scope", { // scope is a special variable, kinda...
      location: "input",
      mangledName: "scope",
      index: scopeIndex
    })
  }

  for (let i = 0; i < inputFormat.length; ++i) {
    let v = inputFormat[i]

    if (typeof v !== 'string') {
      throw new CompilationError(`Input variable at index ${i} is not a string`)
    }

    if (v === "scope") continue
    if (!usedVariables.get(v)) {
      throw new CompilationError(`Input variable ${v} at index ${i} is unused`)
    }

    variableLocations.set(v, {
      location: "input",
      mangledName: v,   // don't mangle input variables
      index: i
    })
  }

  for (let [ varName, varInfo ] of usedVariables.entries()) {
    let definition = varInfo.operatorDefinition
    let existingLocation = variableLocations.get(varName)

    if (definition) {  // variable is to be evaluated, as in a builtin variable
      if (existingLocation) {
        // TODO descriptive
        throw new CompilationError(`Variable ${varName} is a defined constant`)
      }

      variableLocations.set(varName, {
        location: "evaluate",
        operatorDefinition: definition,
        mangledName: genVariableName(),  // mangle to avoid clashes
        constant: definition.tags.constant
      })
    } else {
      if (existingLocation) continue

      if (usesScope) {
        // If a scope is used, assume the variable lies in the scope
        variableLocations.set(varName, {
          location: "scope",
          mangledName: varName  // okay to not mangle this
        })

        continue
      }

      let staticInfo = staticVariables.get(varName)
      if (staticInfo) {
        variableLocations.set(varName, {
          location: "static",
          mangledName: genVariableName(),
          info: staticInfo
        })
      } else {
        // If all else fails, assume static with no information
        variableLocations.set(varName, {
          location: "static",
          mangledName: genVariableName(),
          info: null
        })
      }
    }
  }

  if (doDebug) {
    logVariableLocations(variableLocations, log)
  }

  return {
    usesScope,
    variableLocations
  }
}

function checkInputFormat(inputFormat) {
  if (!Array.isArray(inputFormat)) {
    if (typeof inputFormat !== "string") {
      throw new CompilationError("inputFormat must be an array or string")
    }

    inputFormat = [ inputFormat ]
  }

  for (let i = 0; i < inputFormat.length; ++i) {
    let varName = inputFormat[i]

    for (let j = i + 1; j < inputFormat.length; ++j) {
      if (varName !== inputFormat[j]) {
        throw new CompilationError(`Variable ${varName} is defined twice (at indices ${i} and ${j})`)
      }
    }
  }

  return inputFormat
}

let id = 0

/**
 * Generate a unique variable name that will not conflict with other names
 * @returns {string}
 */
function genVariableName () {
  return "$" + (++id)
}
/**
 *
 * @param inputFormat
 * @param variableLocations
 * @param usesScope
 * @param typechecks
 */
function getVariableRetrieval (inputFormat, variableLocations, usesScope, typechecks) {
  let signature = inputFormat
  let fragment = new CodeFragment()

  if (usesScope && typechecks) {
    // scope typecheck must precede all others
    let scopeIndex = variableLocations.get("scope").index
    fragment.insertFragment(getScopeTypecheck(scopeIndex))
  }

  for (let [ varName, varInfo ] in variableLocations.entries()) {
    let location = varInfo.location
    switch (location) {
      case "evaluate":
      case "static":
        // Will be evaluated or set separately from the main function body (in the preamble)
        fragment.insertFragment(new SpecialFragment("static_variable", varInfo))
        break
      case "scope":
        fragment.insertFragment(`let ${varInfo.mangledName} = scope.${varName};`)
        break
      case "input": // Will be set no matter what
        break
      default:
        throw new CompilationError("?")
    }

    fragment.defines.push(varInfo.mangledName)
  }

  return { signature, fragment }
}
// Write a function which checks whether a string is a valid JS variable name (used for detecting accidental stuff, not
// sanitizing). $ is not allowed.
function isValidVariable (name) {
  return typeof name === "string" && /^[a-zA-Z_][a-zA-Z0-9_]*$/.test(name)
}

function checkVariableNames (variableLocations) {
  for (let varName in variableLocations.values()) {
    if (!isValidVariable(varName)) {
      throw new CompilationError(`Invalid variable name ${varName}`)
    }
  }
}

function resolveConcreteTypes (root, nodeInformation, mode) {
  for (let [ node, info ] of nodeInformation.entries()) {
    let concreteType = mode.getConcreteType(node.type)

    if (!concreteType) {
      throw new CompilationError(`Unable to find concrete type for mathematical type ${node.type} in mode ${mode.name}`)
    }

    info.concreteType = concreteType
  }
}

function resolveConstants (root, nodeInformation, mode) {
  let fragment = new CodeFragment()


}

function resolveEvaluators (root, nodeInformation, mode) {
  // Bottom up; extra
  root.applyAll(node => {
    let info = nodeInformation.get(node)
    let evaluator = null
    let cast = null, def

    if (def = node.operatorDefinition) {
      evaluator = def.getBestEvaluator({

      })
    }

    info.evaluator = evaluator
  }, false, true /* children first */)
}

function compileTarget (root, target, opts) {
  if (typeof target !== "object") {
    throw new CompilationError(`Target description must be a JS object, not ${typeof target}`)
  }

  const doDebug = opts.debug ?? true
  let debugLog = []
  let globalNodeInformation = opts.nodeInformation

  // For debugging purposes
  function log (msg) {
    if (!doDebug) return
    if (typeof msg === "function") { // avoid unnecessary string computations when not debugging
      msg = msg()
    }

    debugLog.push(msg)
  }

  let mode = target.mode
  if (mode === undefined) {
    log("Evaluation mode not given; defaulting to normal")
    mode = "normal"
  }

  try {
    mode = toEvaluationMode(target.mode)
  } catch (e) {
    // Re-raise error
    throw new CompilationError(e.message)
  }

  let inputFormat = target.inputFormat
  inputFormat = checkInputFormat(inputFormat)

  // Compute variable locations; a variable is either provided as an input directly, provided in the scope object,
  // an operator definition, or (if all fails) a static variable. Non-builtin variables are presumed to be in the scope
  // if not explicitly declared to be static
  let staticVariables = opts.staticVariables ?? []
  let usedVariables = opts.usedVariables
  let typechecks = opts.typechecks ?? true

  let { variableLocations, usesScope } = getVariableLocations(staticVariables, usedVariables, inputFormat, log, doDebug)
  checkVariableNames(variableLocations)

  // The evaluation procedure is as follows:
  //  - Get all variables from their respective locations
  //  - If enabled, typecheck and convert variables
  //  - Compute each node in sequence, abstracted as a set of assignments
  //  - Return the result

  let assnGraph = generateAssignmentGraph(root, variableLocations, usesScope, opts)  // Elides identity casts and single-element ASTGroups



  return {
    mode,
    assnGraph,
    debug: debugLog
  }

  let nodeInformation = createInformationMap(root)
  resolveConcreteTypes(root, nodeInformation, mode)  // TODO: prefer the simplest possible type regardless of mode

  // Some constants may need special processing, for example in an arbitrary-precision setting
  let constantFragment = resolveConstants(root, nodeInformation, mode)

  // Resolves evaluators and concrete casts. Most nodes, and every non-leaf node, will have an evaluator and a set of
  // concrete casts (possibly identity casts)
  resolveEvaluators(root, nodeInformation, mode)

  // At this point, everything is (pretty much) an evaluator, besides variables. Constants are to be computed via
  // calling concreteType.castPermissive on their value. Writes evaluators are preferred over new evaluators. Casts
  // are also evaluators.

  //let { signature, fragment: variableRetrievalFragment } = getVariableRetrieval(inputFormat, variableLocations, usesScope, typechecks)

  //let assignmentGraph = getAssignmentGraph()

  return {
    mode,
    nodeInformation,
    debug: debugLog
  }
}

function createInformationMap(root) {
  let m = new Map()
  root.applyAll(node => {
    m.set(node, {})
  })

  return m
}

/**
 * Compile an ASTNode into an evaluable function or functions. This should be preferred for any expression that will be
 * evaluated many times. This function needs to be pretty well optimized...
 * @param root
 * @param modes
 */
export function compileNode(root, {
  targets =
    {
      // Evaluation mode
      mode: "normal",
      // Function call will accept arguments as such. Special values: "scope" is a plain JS object which contains
      // variables as key–value pairs. "scope_map" is a JS Map which contains variables as key–value pairs.
      inputFormat: [ "scope" ],
      // Static variables are actually OperatorDefinitions, since we want them to be evaluable in different modes. For
      // example, pi might be 3.14159... in normal mode, but [3.1415926535897927, 3.1415926535897936] in interval mode.
      // The values of (non-constant) static variables may be explicitly set with the setStatic(name, value) function,
      // which will be returned.... hm.
      // Whether to do typechecks on all inputted variables (might be slightly slower)
      typechecks: true,
      returnMutable: true,  // Whether to allocate a new variable when returning
      returns: "value"
    },
  staticVariables = []
} = {}) {
  if (!(root instanceof ASTNode)) {
    throw new CompilationError("First argument to compileNode must be an ASTNode")
  }

  if (!root.allResolved()) {
    throw new CompilationError("Node types must be resolved with .resolveTypes() first")
  }

  let nodeInformation = createInformationMap(root)
  let usedVariables = root.getVariableDependencies()

  analyzeNode(root, nodeInformation, {})

  let opts = {
    staticVariables,
    nodeInformation,
    usedVariables
  }
  let out = []

  let targetIsArray = Array.isArray(targets)
  if (!targetIsArray) {
    targets = [ targets ]
  }

  for (let target of targets) {
    out.push(compileTarget(root, target, opts))
  }

  if (!targetIsArray) {
    out = out[0]
  }

  return out
}
