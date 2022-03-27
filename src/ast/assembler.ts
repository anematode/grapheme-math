import { AllowedJSPrimitive } from "./evaluator.js";
import { CompilationError, CompileTarget, genVariableName } from "./compile.js";
import { ConcreteAssignmentGraph } from "./assignment_graph.js";
import { ConcreteType } from "./type";

interface AssemblerEnvironment {
  assembler: Assembler
  importFunction: (f: Function) => string
  importValue: (v: any) => string
  addMain: (f: string) => void
  addPreamble: (f: string) => void
  add: (f: string) => void
  enterPreamble: () => void
  enterMain: () => void
  enterFunction: (fName: string) => void
  addExport: (exportName: string, internalName: string) => void,
  addInternalFunction: (args?: Array<string>) => void
}

export class Assembler {
  main: PlainCodeFragment

  // Array of input variables
  inputFormat: Array<string>

  constructor () {
    this.main = new PlainCodeFragment()
    this.inputFormat = []
  }

  add (f: string | CodeFragment) {
    this.main.add(f)
  }

  prepareConcreteGraph(cGraph: ConcreteAssignmentGraph, target: CompileTarget) {
    let neededInputs = Array.from(cGraph.inputNodes())
    let inputFormat = target.inputFormat
    let usesScope = inputFormat.includes("scope")

    let typechecks = target.typechecks

    for (let [ name, node ] of neededInputs) {
      if (inputFormat.includes(name)) {
        // Variable is immediately defined

        if (typechecks) {
          let f = new TypecheckFragment()
          f.name = name
          f.concreteType = node.type
        }
      } else {
        if (usesScope) {
          let f = new VariableDefinitionCodeFragment()
          f.name = name
          f.value = `scope.${name}`
          f.verbatim = true

          this.add(f)
        }
      }
    }

    // First fragment in main is to get the input variables out of the arguments

    for (let [ name, node ] of cGraph.nodesInOrder()) {

    }
  }

  compile (): any {
    type InternalFunction = { args: Array<string>, text: string }

    // Map of closure var names to imported objects to be passed into the closure
    let imports = new Map<string, any>()
    let exports = new Map<string, string>()
    let internalFunctions = new Map<string, InternalFunction>()

    addInternalFunction([], "preamble")
    addInternalFunction([], "main")

    let writingTo: InternalFunction = internalFunctions.get("main")!

    function importObject (o: any): string {
      let name = "$import_" + genVariableName()

      imports.set(name, o)

      return name
    }

    function enterFunction (fName: string) {
      let intF = internalFunctions.get(fName)

      if (!intF) {
        throw new CompilationError(`Unrecognized internal function name ${intF}`)
      }

      writingTo = intF
    }

    function write (s: string) {
      writingTo.text += s
    }

    function add (s: string, fName: string = "") {
      if (fName) {
        let tmp = writingTo

        enterFunction("main")
        write(s)

        writingTo = tmp
      } else {
        write(s)
      }
    }

    function addMain (s: string) {
      add(s, "main")
    }

    function addPreamble (s: string) {
      add (s, "preamble")
    }

    function addInternalFunction (args?: Array<string>, forcedName: string = ""): string {
      let name = forcedName || ("$function_" + genVariableName())

      internalFunctions.set(name, {
        args: [],
        text: ""
      })

      return name
    }

    function addExport (exportName: string, internalName: string) {
      if (exports.get(exportName)) {
        throw new CompilationError(`Duplicate export name ${exportName}`)
      }

      exports.set(exportName, internalName)
    }

    function assembleInternalFunction (fName: string): string {
      let f = internalFunctions.get(fName)
      if (!f) {
        throw new CompilationError(`Failed to compile internal function ${fName} (not found)`)
      }

      let { args, text } = f
      let argsJoined = args.join(', ')

      return `function ${fName}(${argsJoined}) {\n` + text + "}\n"
    }

    let env: AssemblerEnvironment = {
      assembler: this,
      importFunction: importObject,
      importValue: importObject,
      add,
      addInternalFunction,
      addMain,
      addPreamble,
      enterMain: () => {
        enterFunction("main")
      },
      enterPreamble: () => {
        enterFunction("preamble")
      },
      enterFunction,
      addExport
    }

    this.main.compileToEnv(env)

    // The closure has the following structure:
    // function ($import_$1, $import_$2, ...) {
    //   var $2 = $import_$1()
    //   // ... preamble ...
    //   function main (scope, ... input format ...) {
    //      // Function body
    //      return $ret
    //   }
    //   return { evaluate: main }  // exports
    // }

    let importArray: Array<any> = []  // passed arguments to closure
    let importNames: Array<string> = [] // closure argument names

    for (let [ name, o ] of imports.entries()) {
      importArray.push(o)
      importNames.push(name)
    }

    let mainF = internalFunctions.get("main")!
    let preambleF = internalFunctions.get("preamble")!
    mainF.args = this.inputFormat
    preambleF.args = importNames  // unused

    let fsText = ""

    for (let fName of internalFunctions.keys()) {
      fsText += assembleInternalFunction(fName)
    }

    // Build export text
    let exportText = "return {"
    for (let [ name, e ] of exports.entries()) {
      exportText += `${name}: ${e},`
    }
    exportText += "}"

    let fBody = preambleF.text + fsText + exportText

    // Invoke the closure
    let result = (new Function(...importNames, fBody))(importArray)

    return {
      result
    }
  }
}

abstract class CodeFragment {
  preamble: PlainCodeFragment | null // code to be run in the closure, outside the function
  // getContents: () => Array<string | CodeFragment>

  // Convert a fragment into an actual snippet of JS code by calling appropriate functions on env
  compileToEnv: (env: AssemblerEnvironment) => void
}

class TypecheckFragment implements CodeFragment {
  preamble: null
  name: string
  concreteType: ConcreteType

  compileToEnv (env: AssemblerEnvironment) {
    let t = this.concreteType
    let tc = t.typecheck
    let tcv = t.typecheckVerbose

    if (!tc) {
      throw new CompilationError(`No defined typecheck for concrete type ${t.toHashStr()}`)
    }

    let typecheckFast = env.importFunction(tc)
    let typecheckVerbose = tcv ? env.importFunction(tcv) : '' // if no verbose typecheck available, don't include it

    let name = this.name

    env.addMain(`if (${typecheckFast}(${this.name}) {
       
    }`)
  }
}

// Just a group of code fragments
export class PlainCodeFragment implements CodeFragment {
  preamble: PlainCodeFragment | null
  contents: Array<string | CodeFragment>

  constructor () {
    this.contents = []
  }

  add (f: string | CodeFragment) {
    this.contents.push(f)
  }

  compileToEnv (env: AssemblerEnvironment) {
    // Just compile all children

    env.enterPreamble()

    this.preamble?.compileToEnv(env)

    env.enterMain()

    this.contents.map(c => assembleFragment(env, c))
  }
}

export class InvokeVoidFunctionCodeFragment implements CodeFragment {
  preamble: null
  func: Function
  args: Array<string>

  compileToEnv (env: AssemblerEnvironment) {
    let { func, args } = this

    args = args ?? []

    let s = args.map(a => assembleFragment(env, a)).join(', ')

    env.add(`${env.importFunction(func)}(${s})`)
  }
}

function assembleFragment (env: AssemblerEnvironment, f: string | CodeFragment): void {
  (typeof f === "string") ? env.add(f) : f.compileToEnv(env)
}

export class VariableDefinitionCodeFragment implements CodeFragment {
  preamble: null
  name: string

  // If a JS primitive, the arity is implicit in the number of arguments
  func?: Function | AllowedJSPrimitive
  args?: Array<string>  // arguments to the function, to be copied verbatim as JS code

  value?: any
  verbatim?: boolean  // Whether to copy
  construct?: boolean  // Whether to construct a new variable or assign it to an existing name

  compileToEnv (env: AssemblerEnvironment) {
    let { name, func, args, value, verbatim, construct } = this

    args = args ?? []
    let v: string = ""

    if (typeof func === "function") {
      let s = args.join(', ')

      v = `${env.importFunction(func)}(${s})`
      // Comma separated, etc.
    } else if (typeof func === "string") {
      // Primitive
      let len = args.length

      if (len === 0 || len > 2) {
        // TODO specificity
        throw new CompilationError(`Invalid number of arguments (${len}) to primitive ${func}`)
      }

      v = (len === 1) ? `${func}(${args[0]})`  // unary
        : `(${args[0]}) ${func} (${args[1]})` // binary
    } else {
      v = verbatim ? (value + '') : env.importValue(value)
    }

    env.add(`${construct ? "var " : ""}${name} = ${v};`)
  }
}
