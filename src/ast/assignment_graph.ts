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
import { ConcreteEvaluator } from "./evaluator.js";
import { OperatorDefinition } from "./operator_definition.js";
import { ASTNode } from "./node.js";
import { CompilationError } from "./compile";

type NodeBase = {
  // Technically duplicates the information in the Map, but makes things a bit easier. Each node has a unique name,
  // including across function boundaries, except for variables and the special variable $output.
  name: string

  // Whether this node is an input node
  isInput: boolean

  isConditional: false

  // Whether this node originated from a straightforward type cast
  isCast: boolean

  // Names of variable arguments
  args?: Array<string>

  // Corresponding AST node, if applicable
  astNode?: ASTNode

  // Used for constant nodes
  value?: string
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

  // The output node has a special name: $ret. Other variables are referred to by *name*, not by reference.

  // Map node name -> reference
  nodes: Map<string, NodeType>


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
    let stack = ["$ret"] // last element in stack is the element we will recurse into

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

}

export class ConcreteAssignmentGraph extends AssignmentGraph<ConcreteGraphNode> {
  optimize (opts={}) {
    // Crude optimization
  }
}
