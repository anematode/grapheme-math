// An assignment graph is a (mathematical or concrete) abstraction of a set of assignments that would occur in a given
// function evaluation. Unlike an AST, nodes can be shared by other nodes. Consider the following example:
// f(x) = (x+1)^2 + (x+1)
// Input: x: real
// Output: real
// $1 <- x + 1
// $2 <- $1 * $1    (or Math.pow($1, 2))
// ret <- $2 + $1
// Note how multiple lines use the node $1. In an AST, this behavior cannot happen; each node has one and exactly one
// parent. There is an important restriction, however: Each variable can only be assigned once. We have an assignment
// graph for both the mathematical and concrete stages, permitting optimizations at each level, with some degree of
// shared logic.

import { ConcreteType, MathematicalType } from "./type.js";
import { ConcreteEvaluator } from "./concrete_evaluator.js";
import { MathematicalCast, OperatorDefinition } from "./operator_definition.js";
import { ExpressionASTGroup, ExpressionASTNode, ConstantNode, OperatorNode, VariableNode } from "./expression.js";
import { CompilationError, genVariableName } from "./compile.js";

type NodeBase = {
  // Technically duplicates the information in the Map, but makes things a bit easier. Each node has a unique name,
  // including across function boundaries, except for variables and the special variable $ret.
  name: string

  // Whether this node is an input node
  isInput: boolean

  isConditional: false

  // Whether this node originated from a straightforward type cast
  isCast: boolean

  // Names of variable arguments
  args?: string[]

  // Corresponding AST node, if applicable
  astNode?: ExpressionASTNode

  // Used for constant nodes
  value?: any
  stringValue?: string
}

type ConditionalNodeBase = NodeBase & {
  isConditional: true

  // If condition is true, branch1 is taken; if condition is false, branch2 is taken
  branch1: string
  branch2: string
  condition: string
}

export type MathematicalGraphNode = (ConditionalNodeBase | NodeBase) & {
  type: MathematicalType

  // Only operators, and variables with operator definitions, like pi, etc.
  operatorDefinition?: OperatorDefinition
}

export type ConcreteGraphNode = (ConditionalNodeBase | NodeBase) & {
  type: ConcreteType

  evaluator?: ConcreteEvaluator
}

class AssignmentGraph<NodeType extends NodeBase> {
  // We represent a graph as a set of nodes (who'd of thought!) There is a single output node, various input nodes,
  // and each output node has various children as its inputs. The inputs are canonically evaluated left-to-right, and
  // there is a partial order on the nodes; nodes on the same branch are ordered by which one appears first left
  // to right. Nodes on different branches are not guaranteed to both be evaluated in a given instance and therefore
  // have no defined order between them. In particular, order is not transitive.

  // Map node name -> reference
  nodes: Map<string, NodeType>
  root: string  // name of root node

  /**
   * Iterate over input variables—variables which are either static or provided as arguments to the function
   */
  * inputNodes (): Generator<[string, NodeType]> {
    let entries = this.nodes.entries()

    for (let [name, entry] of entries) {
      if (entry.isInput) {
        yield [name, entry]
      }
    }
  }

  * nodesInOrder (): Generator<[string, NodeType]> {
    // Starting at $ret, yield notes from left to right in the order they are needed

    let nodes = this.nodes
    let enteredNodes = new Set<string>()
    let stack = [ this.root ] // last element in stack is the element we will recurse into

    let iters = 0
    const MAX_ITERS = 10000 // prevent infinite loopage

    while (iters < MAX_ITERS && stack.length !== 0) {
      let name = stack[stack.length - 1]
      let node = nodes.get(name)

      if (!node) {
        throw new CompilationError(`Could not find node with name ${name}`)
      }

      if (enteredNodes.has(name)) {
        yield [name, node]
        stack.pop()
      } else {
        if (node.args) {
          let args = node.args

          for (let i = args.length - 1; i >= 0; --i) {
            if (!enteredNodes.has(args[i]))
              stack.push(args[i]) // push children in reverse order, so that we examine the first child first
          }
        }

        enteredNodes.add(name)
      }

      iters++
    }

    if (iters === MAX_ITERS) {
      throw new CompilationError("Infinite loop in assignment graph")
    }
  }
}

export class MathematicalAssignmentGraph extends AssignmentGraph<MathematicalGraphNode> {
  constructFromNode(root: ExpressionASTNode) {
    let assnMap = new Map<string, MathematicalGraphNode>()

    // ASTNode -> graph node name
    let astToGraphMap = new Map<ExpressionASTNode, string>()

    function defineGraphNode(name: string, astNode: ExpressionASTNode | null, info: MathematicalGraphNode) {
      if (astNode) {
        astToGraphMap.set(astNode, name)
      }

      assnMap.set(name, info)
    }

    // Implicitly left to right
    root.applyAll((astNode: ExpressionASTNode) => {
      let gNode: MathematicalGraphNode | null = null
      let name = astToGraphMap.get(astNode) ?? genVariableName()

      switch (astNode.nodeType()) {
        case ExpressionASTNode.TYPES.VariableNode: {
          if (astToGraphMap.get(astNode)) {
            // Only define variables once
            return
          }

          if (!astNode.operatorDefinition) {
            name = (astNode as VariableNode).name

            gNode = {
              name,
              type: astNode.type!,  // must be non-null since it passed null checks earlier
              isConditional: false,
              isCast: false,
              isInput: true,
              astNode
            }

            break
          }
        }
        // Fall through
        case ExpressionASTNode.TYPES.OperatorNode:
          let n = astNode as OperatorNode

          let args: ExpressionASTNode[] = n.children ?? []
          let casts: MathematicalCast[] = (args.length === 0) ? [] : n.casts!

          let castedArgs = casts.map((cast, i) => {
            let arg = args[i]
            let argName = astToGraphMap.get(arg)

            if (!argName) {
              throw new CompilationError("?")
            }

            if (cast.isIdentity()) {
              return argName
            }

            let castedArgName = genVariableName()

            // Create node for the cast
            defineGraphNode(castedArgName, arg, {
              name: castedArgName,
              type: cast.dstType(),
              isConditional: false,
              isCast: true,
              isInput: false,
              args: [ argName ],
              operatorDefinition: cast,
              astNode: arg // for casts, store the argument as the corresponding node
            })

            return castedArgName
          })

          gNode = {
            name,
            type: n.type!,
            isConditional: false,
            isCast: false,
            isInput: false,
            args: castedArgs,
            operatorDefinition: n.operatorDefinition!,
            astNode
          }

          break
        case ExpressionASTNode.TYPES.ASTGroup:
          // Groups are entirely elided by mapping them to the variable name of their only child
          let c = (astNode as ExpressionASTGroup).children[0]
          if (!c) {
            throw new CompilationError("Empty ASTGroup in expression")
          }

          astToGraphMap.set(astNode, astToGraphMap.get(c)!)
          return
        case ExpressionASTNode.TYPES.ConstantNode:
          gNode = {
            name,
            type: astNode.type!,
            isConditional: false,
            isCast: false,
            isInput: false,
            value: (astNode as ConstantNode).value,
            astNode
          }

          break
        case ExpressionASTNode.TYPES.ASTNode:
          throw new CompilationError(`Raw ASTNode in expression`)
      }

      defineGraphNode(name, astNode, gNode)
    }, false /* all children */, true /* children first */)

    this.nodes = assnMap

    let graphRoot = astToGraphMap.get(root)
    if (!graphRoot) {
      throw new CompilationError("?")
    }

    this.root = graphRoot
  }
}

export class ConcreteAssignmentGraph extends AssignmentGraph<ConcreteGraphNode> {
  optimize (opts={}) {
    // Crude optimization
  }
}
