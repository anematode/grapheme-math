import {ASTNode, EvaluationError, VariableDependencies} from "./node.js"
import {EvaluationMode, toEvaluationMode} from "./eval_modes.js"


export class CompilationError extends Error {
  constructor (message) {
    super(message)

    this.name = 'CompilationError'
  }
}

// Throws if an input format is invalid (i.e., if there are two arguments with the same name)
function checkInputFormat(inputFormat: Array<string>) {
  for (let i = 0; i < inputFormat.length; ++i) {
    let varName = inputFormat[i]

    for (let j = i + 1; j < inputFormat.length; ++j) {
      if (varName !== inputFormat[j]) {
        throw new CompilationError(`Variable ${varName} is defined twice (at indices ${i} and ${j})`)
      }
    }
  }
}

let id = 0

/**
 * Generate a unique variable name that will not conflict with other names (locally or globally; no name will ever be
 * returned twice)
 * @returns {string}
 */
function genVariableName (): string {
  return "$" + (++id)
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
  checkInputFormat(inputFormat)
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
