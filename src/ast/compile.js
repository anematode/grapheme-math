import {ASTNode, EvaluationError} from "./node.js"
import {toEvaluationMode} from "./eval_modes.js"

export class CompilationError extends Error {
  constructor (message) {
    super(message)

    this.name = 'CompilationError'
  }
}

class ExpressionAnalysis {

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
        log(() => `Variable ${varName} is to be found in the scope object, which has index ${variableLocations.get("scope").index}`)
        break
      case "input":
        log(() => `Variable ${varName} is to be found at index ${location.index}`)
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

/**
 * Fragments used for special handling, like errors and such
 */
class SpecialFragment {
  constructor (type, opts) {
    this.type = type
    this.opts = opts
  }
}

// A code fragment to be assembled. A very crude representation of actual JS code; it doesn't have to be very
// sophisticated, because it's usually used very late in the compilation process
class CodeFragment {
  constructor () {
    /**
     * Map between variables used within the code fragment and external objects they represent. For example, $1 might
     * be Math.sin or some other evaluator function.
     * @type {Map<string, any>}
     */
    this.externalDependencies = new Map()

    /**
     * Dependencies on predefined variables
     * @type {string[]}
     */
    this.internalDependencies = []

    /**
     * The text of the fragment as a array to be joined
     * @type {Array<string|SpecialFragment>}
     */
    this.text = []

    /**
     * Variables defined after this fragment finishes
     * @type {string[]}
     */
    this.defines = []

    /**
     * Stuff to be run in the preamble (should be a separate code fragment)
     * @type {null|CodeFragment}
     */
    this.preambleFragment = null
  }

  addExternalDep (obj) {
    let id = genVariableName()
    this.externalDependencies.set(id, obj)

    return id
  }

  addInternalDep (name) {
    // A variable with this name must be defined before this code executes for it to work
    this.internalDependencies.push(name)
  }

  insertText (...text) {
    this.text.push(...text)
  }

  getPreamble () {
    let f = this.preambleFragment

    return f ?? (this.preambleFragment = new CodeFragment())
  }

  insertFragment (fragment) {
    if (!fragment) return

    let map = fragment.externalDependencies
    this.insertText(...fragment.text)

    // Merge map and external dependencies
    for (let [ key, value ] of map.entries()) {
      this.externalDependencies.set(key, value)
    }

    let list = fragment.internalDependencies
    // Merge list and internal dependencies
    for (let i = 0; i < list.length; ++i)
      if (!this.internalDependencies.includes(list[i]))
        this.internalDependencies.push(list[i])


    this.insertText(fragment.text)
    for (let i = 0; i < fragment.defines.length; ++i) {
      this.defines.push(fragment.defines[i])
    }

    if (fragment.preambleFragment) {
      this.getPreamble().insertFragment(fragment.preambleFragment)
    }

    return this
  }
}

let id = 0

/**
 * Generate a unique variable name that will not conflict with other names
 * @returns {string}
 */
function genVariableName () {
  return "$" + (++id)
}


function getAssignmentGraph () {

}

function getScopeTypecheck (index) {
  let f = new CodeFragment()

  f.insertText(`if (!(typeof scope === "object")) {`,
    new SpecialFragment("error", { errorType: "type", message: `Scope (at index ${index}) must be an object` })
  `}`)

  return f
}

/**
 *
 * @param inputFormat
 * @param variableLocations
 * @param usesScope
 * @param typecheck
 * @returns {CodeFragment}
 */
function getVariableRetrieval (inputFormat, variableLocations, usesScope, typecheck) {
  let signature = inputFormat
  let fragment = new CodeFragment()

  if (usesScope && typecheck) {
    // scope typecheck must precede all others
    let scopeIndex = variableLocations.get("scope").index
    fragment.insertFragment(getScopeTypecheck(scopeIndex))
  }

  for (let [ varName, varInfo ] in variableLocations.entries()) {
    let location = varInfo.location
    switch (location) {
      case "evaluate":
        // Will be evaluated separately from the main function body (in the preamble)
        fragment.insertFragment(new SpecialFragment("evaluate_variable", { info: varInfo, name: varName }))
        break
      case "static":
        log(() => `Variable ${varName} is static`)
        break
      case "scope":
        log(() => `Variable ${varName} is to be found in the scope object, which has index ${variableLocations.get("scope").index}`)
        break
      case "input":
        log(() => `Variable ${varName} is to be found at index ${location.index}`)
        break
    }
  }
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

function compileTarget (root, target, opts) {
  if (typeof target !== "object") {
    throw new CompilationError(`Target description must be a JS object, not ${typeof target}`)
  }

  const doDebug = opts.debug ?? true
  let debugLog = []
  let nodeInformation = opts.nodeInformation

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
    throw new CompilationError(e.message)
  }

  let inputFormat = target.inputFormat
  inputFormat = checkInputFormat(inputFormat)

  // Compute variable locations; a variable is either provided as an input directly, provided in the scope object,
  // an operator definition, or (if all fails) a static variable. Non-builtin variables are presumed to be in the scope
  // if not explicitly declared to be static
  let staticVariables = opts.staticVariables ?? []
  let usedVariables = opts.usedVariables

  let { variableLocations, usesScope } = getVariableLocations(staticVariables, usedVariables, inputFormat, log, doDebug)
  checkVariableNames(variableLocations)

  // The evaluation procedure is as follows:
  //  - Get all variables from their respective locations
  //  - If enabled, typecheck variables
  //  - Compute each node in sequence, abstracted as a set of assignments
  //  - Return the result

  // This returns the
  let { signature, variableRetrieval } = getVariableRetrieval(inputFormat, variableLocations, usesScope)
  let assignmentGraph = getAssignmentGraph()

  return {
    mode,
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
