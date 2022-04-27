import { AllowedJSPrimitive } from "./evaluator.js";
import { CompilationError, CompileTarget, genVariableName } from "./compile.js";
import { ConcreteAssignmentGraph } from "./assignment_graph.js";
import { ConcreteType } from "./type.js";

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
  addInternalFunction: (args?: string[]) => void
}

export class Assembler {
  preamble: PlainCodeFragment
  main: PlainCodeFragment

  fragments: Map<string, PlainCodeFragment>

  // Array of input variables
  inputFormat: string[]
  inputTypes: (string | ConcreteType)[]
  returns: ConcreteType

  constructor () {
    let preamble = this.preamble = new PlainCodeFragment()
    let main = this.main = new PlainCodeFragment()

    let fs = this.fragments = new Map()
    fs.set("main", main)
    fs.set("preamble", preamble)  // special code fragment

    this.inputFormat = []
  }

  add (f: string | CodeFragment, fragmentName: string = "main") {
    let fragment = (fragmentName === "main") ? this.main : this.fragments.get(fragmentName)

    if (!fragment) {
      throw new CompilationError(`Unknown fragment name ${fragmentName}`)
    }

    fragment.add(f)
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

          // TODO
        }
      } else {
        if (usesScope) {
          let f = new VariableDefinitionCodeFragment()
          f.name = name
          f.value = `scope.${name}`
          f.verbatim = true

          this.add(f)
        } else {
          throw new CompilationError(`Can't find variable ${name}`)
        }
      }
    }

    this.inputFormat = inputFormat

    // First fragment in main is to get the input variables out of the arguments

    for (let [ name, node ] of cGraph.nodesInOrder()) {
      // Don't need to do anything if not input; everything taken care of above
      if (!node.isInput) {
        let ev = node.evaluator
        if (ev) {
          let loc = ev.isConstant ? "preamble" : "main" // put in preamble if it's a constant
          let construct = ev.evalType === "new"

          if (construct || loc === "preamble") {
            let f = new VariableDefinitionCodeFragment()

            f.name = name
            if (ev.primitive) {
              f.func = ev.primitive
            } else {
              f.func = ev.func
            }
            f.args = node.args!
            f.construct = construct


            this.add(f, loc)
          } else {
            // writes evaluator
            let f = new InvokeVoidFunctionCodeFragment()

            f.func = ev.func
            f.args = [ ...node.args!, name ]  // writes to name

            let cr = new VariableDefinitionCodeFragment()

            cr.name = name
            cr.args = []
            cr.func = node.type.init  // create the variable in the preamble
            cr.construct = true

            this.add(f, "main")
            this.add(cr, "preamble")
          }
        } else {
          if (node.value !== undefined) { // Constant
            let n = new VariableDefinitionCodeFragment()

            n.name = name
            n.value = node.value
            n.construct = true

            this.add(n, "preamble")
          }
        }
      }

      if (name === cGraph.root) { // single return statement
        this.add(`return ${cGraph.root}`)
      }
    }

    let inputCTypes = this.inputFormat.map(varName => {
      if (varName === "scope") {
        return "scope"
      }

      let n = cGraph.nodes.get(varName)
      if (!n)  { // Unused input node
        return "any"
      }

      return n.type
    })

    this.inputTypes = inputCTypes
    this.returns = cGraph.nodes.get(cGraph.root)!.type
  }

  compile () {
    type InternalFunction = { args: string[], text: string }

    // Map of closure var names to imported objects to be passed into the closure
    let imports = new Map<any, string>()
    let exports = new Map<string, string>()
    let internalFunctions = new Map<string, InternalFunction>()

    addInternalFunction([], "preamble")
    addInternalFunction([], "main")

    let writingTo: InternalFunction = internalFunctions.get("preamble")!
    write("'use strict';")

    let tagImports = false

    function numberToTag (n: number): string {
      return (n + '').replace(/[.+-]/, '_')
    }

    function importObject (o: any): string {
      let existing = imports.get(o)
      if (existing !== undefined) {
        return existing
      }

      let name = "$import_" + genVariableName()
      if (tagImports) {
        let tag = (typeof o === "function") ? (o.tag ?? o.name) : (typeof o === "number" ? numberToTag(o) : "unknown")
        name += "_" + tag
      }

      imports.set(o, name)

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
        // TODO undef check
        internalFunctions.get(fName)!.text += s
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

    function addInternalFunction (args?: string[], forcedName: string = ""): string {
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

    enterFunction("main")
    this.main.compileToEnv(env)

    // Preamble compilation should occur after all internal compilations
    enterFunction("preamble")
    this.preamble.compileToEnv(env)

    let mainF = internalFunctions.get("main")!
    let preambleF = internalFunctions.get("preamble")!

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

    let importArray: any[] = []  // passed arguments to closure
    let importNames: string[] = [] // closure argument names

    for (let [ o, name ] of imports.entries()) {
      importArray.push(o)
      importNames.push(name)
    }

    mainF.args = this.inputFormat
    preambleF.args = importNames  // unused

    let fsText = ""

    for (let fName of internalFunctions.keys()) {
      if (fName !== "preamble")
        fsText += assembleInternalFunction(fName)
    }

    addExport("evaluate", "main")

    // Build export text
    let exportText = "return {"
    for (let [ name, e ] of exports.entries()) {
      exportText += `${name}: ${e},`
    }
    exportText += "}"

    let fBody = preambleF.text + fsText + exportText
    //console.log(fBody)

    // Invoke the closure
    let result = (new Function(...importNames, fBody)).apply(null, importArray) as Function

    return {
      result,
      inputFormat: this.inputFormat,
      inputTypes: this.inputTypes,
      returns: this.returns
    }
  }
}

abstract class CodeFragment {
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
    let typecheckVerbose = tcv ? env.importFunction(tcv) : "" // if no verbose typecheck available, don't include it

    let name = this.name

    /*env.addMain(`if (${typecheckFast}(${this.name}) {

    }`)*/
  }
}

// Just a group of code fragments
export class PlainCodeFragment implements CodeFragment {
  contents: (string | CodeFragment)[]

  constructor () {
    this.contents = []
  }

  add (f: string | CodeFragment) {
    this.contents.push(f)
  }

  compileToEnv (env: AssemblerEnvironment) {
    // Just compile all children
    this.contents.map(c => assembleFragment(env, c))
  }
}

export class InvokeVoidFunctionCodeFragment implements CodeFragment {
  func: Function
  args: string[]

  compileToEnv (env: AssemblerEnvironment) {
    let { func, args } = this

    args = args ?? []

    let s = args.join(', ')

    env.add(`${env.importFunction(func)}(${s});\n`)
  }
}

function assembleFragment (env: AssemblerEnvironment, f: string | CodeFragment): void {
  (typeof f === "string") ? env.add(f) : f.compileToEnv(env)
}

export class VariableDefinitionCodeFragment implements CodeFragment {
  name: string

  // If a JS primitive, the arity is implicit in the number of arguments
  func?: Function | AllowedJSPrimitive
  args?: string[]  // arguments to the function, to be copied verbatim as JS code

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

    env.add(`${construct ? "var " : ""}${name} = ${v};\n`)
  }
}
