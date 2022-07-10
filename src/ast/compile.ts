import {
  ExpressionASTGroup,
  ExpressionASTNode,
  ConstantNode,
  EvaluationError,
  OperatorNode, ResolveTypesOptions,
  VariableDependencies,
  VariableNode
} from "./expression.js"
import {EvaluationMode, toEvaluationMode} from "./eval_modes.js"
import {
  ConcreteAssignmentGraph,
  ConcreteGraphNode,
  MathematicalAssignmentGraph,
  MathematicalGraphNode
} from "./assignment_graph.js";
import { MathematicalCast } from "./operator_definition.js";
import { ConcreteType, MathematicalType } from "./type.js";
import { Assembler } from "./assembler.js";
import { parseExpression } from "./parse_expression.js";

export class CompilationError extends Error {
  constructor (message) {
    super(message)

    this.name = 'CompilationError'
  }
}

let id = 0

/**
 * Generate a unique variable name that will not conflict with other names (locally or globally; no name will ever be
 * returned twice)
 * @returns {string}
 */
export function genVariableName (): string {
  return "$" + (++id)
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
  inputFormat?: string | string[]
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

export type CompileTarget = {
  mode: EvaluationMode
  inputFormat: string[]
  typechecks: boolean
  returnNew: boolean
  staticVariables: string[]
  usedVariables: VariableDependencies
}

export type CompileNodeOptions = {
  /**
   * An explicit list of compile targets. If empty, a single target will be outputted
   */
  targets?: CompileTargetOptions | CompileTargetOptions[]
  /**
   * List of variables considered static, augmented by built-in static variables (e.g., pi, i)
   */
  staticVariables?: string[]
  /**
   * Whether to perform typechecks (may be overridden in specific targets)
   */
  typechecks?: boolean
  /**
   * Whether to return a new instance of non primitive types each time (may be overridden in specific targets)
   */
  returnNew?: boolean
  /**
   * List of variables and their mathematical types (may not be overridden in specific targets)
   */
  variables?: {[key: string]: (string | MathematicalType)}
  /**
   * Specification for how mathematical types should be resolved (may not be overridden in specific targets)
   */
  resolveTypes?: ResolveTypesOptions
  /**
   * Input format (may be overridden in a specific target)
   */
  inputFormat?: string[]
  /**
   * Evaluation mode (may be overridden in a specific target)
   */
  mode?: string | EvaluationMode
}

type FilledCompileNodeOptions = {
  targets: CompileTarget | CompileTarget[]
  staticVariables: string[]
}

type RootNodeProperties = {
  usedVariables: VariableDependencies
}

export class CompileTargetResult {
  evaluate: Function

  /**
   * Input variables as a list of strings (for example, f(x,y)
   */
  inputFormat: string[]

  /**
   * Input types as an array of ConcreteTypes, or a string in the case of "scope". In other words, scope is not (for
   * now) considered a ConcreteType
   */
  inputTypes: (ConcreteType | string)[]  // TODO make scope a special concrete type?

  /**
   * Return type
   */
  returns: ConcreteType

  /**
   * Properties of the target (evaluation mode, etc.)
   */
  properties: CompileTarget
  usesScope: boolean

  constructor(evaluate: Function, inputFormat: string[], inputTypes: (ConcreteType | string)[], returns: ConcreteType, properties: CompileTarget) {
    this.evaluate = evaluate
    this.inputFormat = inputFormat
    this.inputTypes = inputTypes
    this.returns = returns
    this.properties = properties

    this.usesScope = inputFormat.includes("scope")
  }
}

/**
 * Result of calling compileNode
 */
export class CompileNodeResult {
  rootNode: ExpressionASTNode
  targets: CompileTargetResult[]

  constructor(root: ExpressionASTNode, targets: CompileTargetResult[]) {
    this.rootNode = root
    this.targets = targets
  }
}

const defaultTarget: CompileTargetOptions = {
  mode: "normal",
  inputFormat: "scope",
  typechecks: true,
  returnNew: true
}

// -1 if fine, otherwise, index of problematic
function checkStringArray (o: any[]): number {
  for (let i = 0; i < o.length; ++i) {
    if (typeof o[i] !== "string") {
      return i
    }
  }

  return -1
}

// Throws if an input format is invalid (i.e., if there are two arguments with the same name)
function checkInputFormat(inputFormat: string[]) {
  let p = checkStringArray(inputFormat)
  if (p !== -1) {
    throw new CompilationError(`Provided variable name at index ${p} in input format is not a string`)
  }

  for (let i = 0; i < inputFormat.length; ++i) {
    let varName = inputFormat[i]

    for (let j = i + 1; j < inputFormat.length; ++j) {
      if (varName === inputFormat[j]) {
        throw new CompilationError(`Variable ${varName} is given twice (at indices ${i} and ${j})`)
      }
    }
  }
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

  inputFormat = inputFormat as string[]
  checkInputFormat(inputFormat)

  if (!Array.isArray(staticVariables)) {
    throw new CompilationError(`Static variables must be an array`)
  }
  let p = checkStringArray(staticVariables)
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

function createAssnGraph(root: ExpressionASTNode): MathematicalAssignmentGraph {
  let graph = new MathematicalAssignmentGraph()
  graph.constructFromNode(root)
  return graph
}

/**
 * Attempt to convert a mathematical assignment graph into a concrete graph
 * @param mGraph Mathematically processed and optimized graph
 * @param target (Filled) compile target
 * @param index Index in the passed target array
 */
function concretizeAssnGraph(mGraph: MathematicalAssignmentGraph, target: CompileTarget, index: number): ConcreteAssignmentGraph {
  let cNodes: Map<string, ConcreteGraphNode> = new Map()

  let mode = target.mode

  function defineGraphNode(name: string, cNode: ConcreteGraphNode) {
    cNodes.set(name, cNode)
  }

  function raiseNoConcreteType(mType: MathematicalType): never {
    throw new CompilationError(`No concrete type found in mode ${mode.toString()} for mathematical type ${mType.toHashStr()}`)
  }

  // Compute concrete types of input nodes first
  for (let [ name, mNode ] of mGraph.inputNodes()) {
    let mType = mNode.type
    let cType = mode.getConcreteType(mType)

    if (!cType)
      raiseNoConcreteType(mType)

    defineGraphNode(name, {
      name,
      type: cType,
      isConditional: mNode.isConditional,
      isCast: mNode.isCast,
      isInput: mNode.isInput,
      astNode: mNode.astNode
    })
  }

  for (let [ name, mNode ] of mGraph.nodesInOrder()) {
    let mType = mNode.type
    let cType = mode.getConcreteType(mType)

    if (!cType)
      raiseNoConcreteType(mType)

    let o = mNode.operatorDefinition
    if (o) {
      let argTypes = mNode.args!.map(s => {
        let cNode = cNodes.get(s)
        if (!cNode)
          throw new CompilationError("?")

        return cNode.type
      })

      let evalType: "writes" | "new" = "writes" // preferred
      if (name === "$ret") {
        if (target.returnNew) { // if returning, prefer a new evaluator (for efficiency's sake)
          evalType = "new"
        }
      }

      let evaluator = o.findEvaluator(argTypes, { evalType })
      if (!evaluator) {
        throw new CompilationError(`Unable to find evaluator ${o.prettyPrint()} in mode ${mode.toString()}, accepting concrete types (${argTypes.map(c => c.toHashStr()).join(' ')})`)
      }

      defineGraphNode(name, {
        name,
        type: cType,
        isConditional: mNode.isConditional,
        isCast: mNode.isCast,
        isInput: mNode.isInput,
        evaluator,
        args: mNode.args,
        astNode: mNode.astNode
      })
    } else {
      defineGraphNode(name, {
        name,
        type: cType,
        isConditional: mNode.isConditional,
        isCast: mNode.isCast,
        isInput: mNode.isInput,
        astNode: mNode.astNode,
        stringValue: mNode.value,  // string value is kept for cases like multiprecision
        value: cType.castPermissive(mNode.value)
      })
    }
  }

  let graph = new ConcreteAssignmentGraph()
  graph.nodes = cNodes
  graph.root = mGraph.root

  return graph
}

/**
 * Compile an ASTNode into an evaluable function or functions. This should be preferred for any expression that will be
 * evaluated many times, because although compilation takes some time, the benefit is quickly reached after even a few
 * thousand evaluations. The traversal of the full AST is expensive!
 * @param root Node to compile
 * @param options List of options
 * @return Returns, among other things, a list of targets, which reflect the targets specified in the compilation opts.
 * If no targets are specified, or the options for the target are given at the topic level rather than in a list of
 * targets, a target array with a single target is returned.
 */
export function compileNode(root: ExpressionASTNode | string, options: CompileNodeOptions = {}): CompileNodeResult {
  if (!(root instanceof ExpressionASTNode)) {
    if (!(typeof root === "string"))
      throw new CompilationError("First argument to compileNode must be an ASTNode or string")

    root = parseExpression(root)
  }

  // Resolve nodes if necessary
  if (!root.allResolved()) {
    root.resolveTypes(options.variables ?? {}, { ...options.resolveTypes, throwOnUnresolved: true })
  }

  // Uniformize properties
  let targetOpts = options.targets
  if (!targetOpts) targetOpts = {}

  let dt = { ...defaultTarget }

  if (options.inputFormat) dt.inputFormat = options.inputFormat
  if (options.mode) dt.mode = options.mode
  if ('typechecks' in options) dt.typechecks = options.typechecks

  if (!Array.isArray(targetOpts)) targetOpts = [ targetOpts ]   // default is a single target (targets[0])

  let rootProperties = {
    usedVariables: root.getVariableDependencies()
  }

  targetOpts = targetOpts as CompileTargetOptions[]

  // Convert each target to a full target
  let targets: CompileTarget[] = []
  for (let i = 0; i < targetOpts.length; ++i) {
    // Merge default options
    let to = targetOpts[i]
    to = Object.assign({ ...dt }, to)

    targets.push(fillTargetOptions(options, to, rootProperties, i))
  }

  let mAssignmentGraph = createAssnGraph(root)
  let compiledResults: CompileTargetResult | CompileTargetResult[] = []

  for (let i = 0; i < targets.length; ++i) {
    let target = targets[i]

    let cAssignmentGraph = concretizeAssnGraph(mAssignmentGraph, target, i)

    cAssignmentGraph.optimize()

    // The crude procedure to build a target is now as follows:
    // - Variable retrieval from the input
    // - Typechecks, if desired
    // - Compiled assignment graph
    // - Return $ret

    let assembler = new Assembler()
    assembler.prepareConcreteGraph(cAssignmentGraph, target)

    let compiled = assembler.compile()
    let result = new CompileTargetResult((compiled.result as any).evaluate,
      compiled.inputFormat,
      compiled.inputTypes,
      compiled.returns,
      target)

    compiledResults.push(result)
  }

  return new CompileNodeResult(root.clone(), compiledResults)
}
