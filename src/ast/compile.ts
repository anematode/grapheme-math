import {ASTNode, EvaluationError, VariableDependencies} from "./node.js"
import {EvaluationMode, toEvaluationMode} from "./eval_modes.js"


export class CompilationError extends Error {
  constructor (message) {
    super(message)

    this.name = 'CompilationError'
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
 * Information about a function to compile
 */
export type CompileTargetOptions = {
  /**
   * Name of an evaluation mode or the mode itself (e.g., "normal", "fast_interval")
   * @defaultValue "normal"
   */
  mode?: string | EvaluationMode,
  /**
   * Array of variable names of the inputs to the function, or a single variable name. "scope" is a special variable
   * indicating an object with keys with the variables in them; any variables not specified to be somewhere else in
   * the input format are expected to either be static (staticVariables) or in the scope.
   * @defaultValue "scope"
   */
  inputFormat?: string | Array<string>
  /**
   * Whether to do typechecks on all inputted variables, incurring a slight runtime cost.
   * @defaultValue true
   */
  typechecks?: boolean,
  /**
   * If returning a complex type, whether to return a new instance of that type each time, or a reused one (to avoid
   * unnecessary allocations). For example, if a function returns a new Complex, it will allocate a new Complex each
   * time, which the user can use freely without worrying its value will change. If it uses an existing Complex, no
   * new allocation will be done and the value may change if the function is invoked again
   * @defaultValue true
   */
  returnNew?: boolean
}

type CompileTarget = {
  mode: EvaluationMode
  inputFormat: Array<string>
  typechecks: boolean
  returnNew: boolean
  staticVariables: Array<string>
  usedVariables: VariableDependencies
}

type CompileNodeOptions = {
  targets?: CompileTargetOptions | Array<CompileTargetOptions>
  staticVariables?: Array<string>
  typechecks?: boolean
  returnNew?: boolean
}

type FilledCompileNodeOptions = {
  targets: CompileTarget | Array<CompileTarget>
  staticVariables: Array<string>
}

type RootNodeProperties = {
  usedVariables: VariableDependencies
}

const defaultTarget = {
  mode: "normal",
  inputFormat: "scope",
  typechecks: true,
  returnNew: true
}

// -1 if fine, otherwise, index of problematic
function checkStringArray (o: Array<any>): number {
  for (let i = 0; i < o.length; ++i) {
    if (typeof o[i] !== "string") {
      return i
    }
  }

  return -1
}

function fillTargetOptions(nodeOpts: CompileNodeOptions, opts: CompileTargetOptions, rootProperties: RootNodeProperties, index: number): CompileTarget {
  if (typeof opts !== "object") {
    throw new CompilationError(`Provided target option at index ${index} is not an object`)
  }

  let givenMode = opts.mode ?? "normal"
  let mode = toEvaluationMode(givenMode, true /* throw on error */)!

  let typechecks = opts.typechecks ?? nodeOpts.typechecks ?? true
  let returnNew = opts.returnNew ?? nodeOpts.returnNew ?? true
  let inputFormat = opts.inputFormat ?? "scope"
  let staticVariables = nodeOpts.staticVariables ?? []

  if (typeof inputFormat === "string") {
    inputFormat = [ inputFormat ]
  }

  inputFormat = inputFormat as Array<string>
  let p = checkStringArray(inputFormat)
  if (p !== -1) {
    throw new CompilationError(`Provided variable name at index ${p} in input format is not a string`)
  }
  if (!Array.isArray(staticVariables)) {
    throw new CompilationError(`Static variables must be an array`)
  }
  p = checkStringArray(staticVariables)
  if (p !== -1) {
    throw new CompilationError(`Provided variable name at index ${p} in input format is not a string`)
  }

  return {
    mode: mode,
    typechecks,
    returnNew,
    inputFormat,
    staticVariables,
    usedVariables: rootProperties.usedVariables
  }
}

/**
 * Compile an ASTNode into an evaluable function or functions. This should be preferred for any expression that will be
 * evaluated many times. This function needs to be pretty well optimized...
 * @param root
 * @param options
 */
export function compileNode(root: ASTNode, options: CompileNodeOptions = {}) {
  if (!(root instanceof ASTNode))
    throw new CompilationError("First argument to compileNode must be an ASTNode")
  if (!root.allResolved())
    throw new CompilationError("Node types must be resolved with .resolveTypes() first")

  let targetOpts = options.targets
  if (!targetOpts) {
    targetOpts = defaultTarget
  }

  if (!Array.isArray(targetOpts)) {
    targetOpts = [ targetOpts ]
  }

  let rootProperties = {
    usedVariables: root.getVariableDependencies()
  }

  targetOpts = targetOpts as Array<CompileTargetOptions>

  // Convert each target to a full target
  let targets: Array<CompileTarget> = []
  for (let i = 0; i < targetOpts.length; ++i) {
    let to = targetOpts[i]

    targets.push(fillTargetOptions(options, to, rootProperties, i))
  }

  console.log(targets)


  let nodeInformation = createInformationMap(root)
  let usedVariables = root.getVariableDependencies()

}
