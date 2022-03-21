/**
 * In this file, we convert strings representing expressions in Grapheme into their ASTNode counterparts. For example,
 * x^2 is compiled to OperatorNode{operator=^, children=[VariableNode{name="x"}, ConstantNode{value="2"}]}
 */
import {
  ConstantNode,
  VariableNode,
  OperatorNode,
  ASTGroup,
  ASTNode
} from './node.js'
import { toMathematicalType } from './builtin/builtin_types.js'

export class ParserError extends Error {
  constructor (message) {
    super(message)

    this.name = 'ParserError'
  }
}

type BaseToken = {
  index: number
}

type ParenToken = BaseToken & {
  type: "paren"
  paren: '(' | ')' | '[' | ']'
}

type ConstantToken = BaseToken & {
  type: "constant"
  value: string
}

type OperatorToken = BaseToken & {
  type: "operator"
  op: string
}

type FunctionToken = BaseToken & {
  type: "function"
  name: string
}

type CommaToken = BaseToken & {
  type: "comma"
}

type VariableToken = BaseToken & {
  type: "variable"
  name: string
}

type StringToken = BaseToken & {
  type: "string"
  contents: string
}

type Token = ParenToken | ConstantToken | OperatorToken | FunctionToken | CommaToken | VariableToken | StringToken

type ASTNodeInfo = any
type UnprocessedASTNodeType = "generic" | "constant" | "variable" | "operator" | "group"
type UnprocessedChildren = Array<UnprocessedASTNode|Token>

class UnprocessedASTNode {
  type: UnprocessedASTNodeType
  info?: ASTNodeInfo
  children: UnprocessedChildren

  constructor (type: UnprocessedASTNodeType, info: ASTNodeInfo, children: UnprocessedChildren) {
    this.type = type
    this.info = info
    this.children = children
  }

  applyAll (f: (UnprocessedASTNode) => void, childrenFirst=false) {
    let children = this.children
    if (!childrenFirst) f(this)
    for (let i = 0; i < children.length; ++i) {
      let child = children[i]
      if (child instanceof UnprocessedASTNode) {
        f(child)

        child.applyAll(f, childrenFirst)
      }
    }
    if (childrenFirst) f(this)
  }
}

type ParserErrorInfo = null | {
  index: number
} | {
  token: Token
  endToken?: Token
}

const operator_regex = /^[*\-\/+^]|^[<>]=?|^[=!]=|^and\s+|^or\s+/
const function_regex = /^([a-zA-Z_][a-zA-Z0-9_]*)\(/  // functions may only use (, [ is reserved for indexing
const constant_regex = /^[0-9]*\.?[0-9]*e?[0-9]+/
const variable_regex = /^[a-zA-Z_][a-zA-Z0-9_]*/
const paren_regex = /^[()\[\]]/
const comma_regex = /^,/
const string_regex = /^"(?:[^"\\]|\\.)*"/

/**
 * Helper function to throw an error at a specific index in a string.
 * TODO make signature uniform
 * @param string Erroneous parsed string
 * @param info The token in the string where the error occurred, ideally with an index attribute
 * @param message The raw error message, to be combined with contextual information
 * @param noIndex If true, provide no index
 */
export function raiseParserError (string: string, info: ParserErrorInfo, message: string = "", noIndex: boolean =false) {
  let index = -1, token = null, endToken = null
  if (info !== null) {
    if ('index' in info) {
      index = info.index
    } else {
      token = info.token
      endToken ??= info.endToken

      index = token.index
    }
  }

  if (!noIndex) // can't use an index if we have no index information
    noIndex = index === -1

  let spaces = ' '.repeat(index)
  let errorLen = (endToken?.index ?? index) - index + 1

  throw new ParserError(
    'Malformed expression; ' + message + (noIndex ? '' : ' at index ' + index + ':\n' + string + '\n' + spaces + '^'.repeat(errorLen))
  )
}

function checkParensBalanced (s: string) {
  // TODO: Handle strings (tokens)

  const parenStack: Array<string> = []

  let i = 0
  let err = false

  outer: for (; i < s.length; ++i) {
    let chr = s[i]

    switch (chr) {
      case '(':
      case '[':
        parenStack.push(chr)
        break
      case ')':
      case ']':
        if (parenStack.length === 0) {
          err = true
          break outer
        }

        if (chr === ')') {
          let pop = parenStack.pop()

          if (pop !== '(') {
            err = true
            break outer
          }
        } else {
          let pop = parenStack.pop()

          if (pop !== '[') {
            err = true
            break outer
          }
        }
    }
  }

  if (parenStack.length !== 0) err = true

  if (err) raiseParserError(s, { index: i }, 'unbalanced parentheses/brackets')
}

// Exclude valid variables if needed later
export function isValidVariableName (str) {
  return true
}

const trimRight = ('trimRight' in String.prototype) ? (s: string): string => (s as any).trimRight() : (s: string) => {
  return s.replace(/\s+$/, '')
}

function getTokens (s: string): Array<Token> {
  let i = 0
  let original_string = s

  s = trimRight(s)
  let prev_len = s.length

  let tokens: Array<Token> = []

  while (s) {
    s = s.trim() // repeatedly trim off whitespace and grab the next token TODO: optimize to simply iterate

    i += prev_len - s.length
    prev_len = s.length

    let match

    do {
      match = s.match(paren_regex)

      if (match) {
        tokens.push({
          type: 'paren',
          paren: match[0],
          index: i
        })

        break
      }

      match = s.match(constant_regex)

      if (match) {
        tokens.push({
          type: 'constant',
          value: match[0],
          index: i
        })

        break
      }

      match = s.match(operator_regex)

      if (match) {
        tokens.push({
          type: 'operator',
          op: match[0].replace(/\s+/g, ''),
          index: i
        })

        break
      }

      match = s.match(comma_regex)

      if (match) {
        tokens.push({
          type: 'comma',
          index: i
        })

        break
      }

      match = s.match(function_regex)

      if (match) {
        // First group is the function name, second group is the type of parenthesis (bracket or open)

        tokens.push({
          type: 'function',
          name: match[1],
          index: i
        })

        tokens.push({
          type: 'paren',
          paren: '(',
          index: i + match[1].length
        })

        break
      }

      match = s.match(variable_regex)

      if (match) {
        tokens.push({
          type: 'variable',
          name: match[0],
          index: i
        })

        break
      }

      match = s.match(string_regex)

      if (match) {
        tokens.push({
          type: 'string',
          contents: match[0].slice(1, -1),
          index: i
        })

        // fall through
      }

      raiseParserError(original_string, { index: i }, 'unrecognized token')
    } while (false)

    let len = match[0].length

    s = s.slice(len) // rm token
  }

  return tokens
}

function checkValid (tokens, string) {
  if (tokens.length === 0) {
    raiseParserError(string, { index: 0 }, 'empty expression', true)
  }

  for (let i = 0; i < tokens.length - 1; ++i) {
    let token1 = tokens[i]
    let token2 = tokens[i + 1]

    let token2IsUnary = token2.op === '-' || token2.op === '+'

    if (
      (token1.type === 'operator' || token1.type === 'comma') &&
      (token2.type === 'operator' || token2.type === 'comma') &&
      (!token2IsUnary || i === tokens.length - 2)
    ) {
      raiseParserError(string, token2, 'two consecutive operators')
    }
    if (token1.paren === '(' && token2.paren === ')')
      raiseParserError(string, token2, 'empty parentheses not associated with function call')
    if (token1.paren === '[' && token2.paren === ']')
      raiseParserError(string, token2, 'empty brackets not associated with function call')
    if (token1.type === 'operator' && token2.paren === ')')
      raiseParserError(
        string,
        token2,
        'operator followed by closing parenthesis'
      )
    if (token1.type === 'operator' && token2.paren === ']')
      raiseParserError(
        string,
        token2,
        'operator followed by closing bracket'
      )
    if (token1.type === 'comma' && token2.paren === ')')
      raiseParserError(
        string,
        token2,
        'comma followed by closing parenthesis'
      )
    if (token1.type === 'comma' && token2.paren === ']')
      raiseParserError(string, token2, 'comma followed by closing bracket')
    if (token1.paren === '(' && token2.type === 'comma')
      raiseParserError(string, token2, 'comma after open parenthesis')
    if (token1.paren === '[' && token2.type === 'comma')
      raiseParserError(string, token2, 'comma after starting bracket')
    if (token1.paren === '(' && token2.type === 'operator' && !token2IsUnary)
      raiseParserError(string, token2, 'operator after starting parenthesis')
    if (token1.paren === '[' && token2.type === 'operator' && !token2IsUnary)
      raiseParserError(string, token2, 'operator after starting bracket')
  }

  if (
    tokens[0].type === 'comma' ||
    (tokens[0].type === 'operator' &&
      !(tokens[0].op === '-' || tokens[0].op === '+'))
  )
    raiseParserError(string, { index: 0 }, 'expression begins with comma or operator')

  const last_token = tokens[tokens.length - 1]
  if (last_token.type === 'comma' || last_token.type === 'operator')
    raiseParserError(string, last_token, 'expression ends with comma or operator')
}

/**
 * Find a pair of parentheses in a list of tokens, namely the first one as indexed by the closing paren/bracket. For
 * example, in (x(y(z)(w))) it will find (z), returning [ paren1 index, paren2 index, paren1 token, paren2 token ]
 */
function findParenIndices (children: Array<Token>): [ number, number, Token, Token ] | null {
  let startIndex = -1
  let startToken: Token | null = null

  for (let i = 0; i < children.length; ++i) {
    let child = children[i]
    if (!('paren' in child)) continue

    if (child.paren === '(' || child.paren === '[') {
      startIndex = i
      startToken = child
    }

    if ((child.paren === ')' || child.paren === ']') && startIndex !== -1)
      return [ startIndex, i, startToken, child ]
  }

  return null
}

/**
 * Given a string like "1.5", "3e10", etc., determine whether it is an integer without evaluating it. Assumes the string
 * is well-formed.
 */
function isStringInteger (s: string): boolean {
  if (s[0] === '-') s = s.slice(1) // trim leading '-'

  let exponent = 0, mIntTrailingZeros = Infinity, mFracLen = 0
  let e = s.indexOf('e')

  // If mFracLen = 0 (no fractional part) and mIntTrailingZeros = 0 (no integer part), the result is 0, so integer
  // If mFracLen > 0 (fractional part), integer if exponent >= mFracLen
  // If mFracLen = 0 (no fractional part), integer if exponent >= -mIntTrailingZeros

  if (e !== -1) { // get exponent
    exponent = parseInt(s.slice(e + 1))

    if (Number.isNaN(exponent)) throw new Error("unrecognized exponent " + s.slice(e + 1))
  } else {
    e = s.length
  }

  let p = s.indexOf('.')
  if (p !== -1) {
    for (let i = e - 1; i > p; --i) {
      // find trailing zeros
      if (s[i] !== '0') {
        mFracLen = i - p
        break
      }
    }
  } else {
    p = e
  }

  for (let i = p - 1; i >= 0; --i) {
    if (s[i] !== '0') {
      mIntTrailingZeros = p - i
    }
  }

  if (mFracLen !== 0) {
    return exponent >= mFracLen
  } else {
    if (mIntTrailingZeros !== 0) {
      return exponent > -mIntTrailingZeros
    } else {
      // 0
      return true
    }
  }
}

/**
 * Convert constants and variables to their ASTNode counterparts
 */
function processConstantsAndVariables (tokens: Array<Token|ASTNode>) {
  for (let i = 0; i < tokens.length; ++i) {
    let token = tokens[i]
    let node

    if ('type' in token) {
      switch (token.type) {
        case 'constant':
          node = new ConstantNode({value: token.value})
          node.type = toMathematicalType(isStringInteger(token.value) ? 'int' : 'real')

          break
        case 'variable':
          node = new VariableNode({name: token.name})
          break
        default:
          continue
      }

      node.info.token = node.info.startToken = node.info.endToken = token
      tokens[i] = node
    }
  }
}

// To process parentheses, we find pairs of them and combine them into ASTNodes containing the nodes and
// tokens between them. We already know the parentheses are balanced, which is a huge help here. We basically go
// through each node recursively and convert all paren pairs to a node, then recurse into those new nodes
function processParentheses (rootNode: ASTNode) {
  rootNode.applyAll(node => {
    let parensRemaining = true
    while (parensRemaining) {
      parensRemaining = false
      let indices = findParenIndices(node.children)

      if (indices) {
        parensRemaining = true

        let [ startIndex, endIndex, startToken, endToken ] = indices

        let newNode = new ASTGroup()
        let expr = node.children.splice(
          startIndex,
          endIndex - startIndex + 1,
          newNode
        )

        newNode.children = expr.slice(1, expr.length - 1)
        newNode.info.token = newNode.info.startToken = startToken
        newNode.info.endToken = endToken
      }
    }
  }, true)
}

// Turn function tokens followed by ASTNodes into OperatorNodes
function processFunctions (rootNode: ASTNode) {
  rootNode.applyAll(node => {
    let children = node.children

    for (let i = 0; i < children.length; ++i) {
      let token = children[i]

      if (token.type === 'function') {
        let newNode = new OperatorNode({ name: token.name })

        children[i] = newNode

        let nextNode = children[i + 1]
        if (!nextNode) {
          throw new Error("Unknown error")
        }

        // Take children from the node coming immediately after
        newNode.children = nextNode.children

        newNode.info.token = newNode.info.startToken = token
        newNode.info.endToken = nextNode.info.endToken
        newNode.info.startExprToken = nextNode.info.startToken
        newNode.info.isFunction = true

        // Remove the node immediately after
        children.splice(i + 1, 1)
      }
    }
  }, true)
}

// Given a node and an index i of a binary operator, combine the nodes immediately to the left and right of the node
// into a single binary operator
function combineBinaryOperator (node: ASTNode, i: number) {
  const children = node.children
  let newNode = new OperatorNode({ name: children[i].op })

  newNode.children = [children[i - 1], children[i + 1]]

  children.splice(i - 1, 3, newNode)
}

// Process the highest precedence operators. Note that e^x^2 = (e^x)^2 and e^-x^2 = e^(-x^2).
function processUnaryAndExponentiation (root: ASTNode) {
  root.applyAll(node => {
    let children = node.children

    // We iterate backwards
    for (let i = children.length - 1; i >= 0; --i) {
      let child = children[i]
      if (child instanceof ASTNode || !child.op) continue

      if (child.op === '-') {
        // If the preceding token is an unprocessed non-operator token, or node, then it's a binary expression
        if (i !== 0 && children[i - 1].type !== 'operator') continue

        let newNode = new OperatorNode({ name: '-' })
        newNode.children = [children[i + 1]]

        children.splice(i, 2, newNode)
      } else if (child.op === '+') {
        // See above
        if (i !== 0 && children[i - 1].type !== 'operator') continue

        // Unary + is considered a no-op
        children.splice(i, 1)
      } else if (child.op === '^') {
        combineBinaryOperator(node, i)

        --i
      }
    }
  }, true)
}

// Combine binary operators, going from left to right, with equal precedence for all
function processOperators (root: ASTNode, operators: Array<string>) {
  root.applyAll(node => {
    let children = node.children

    for (let i = 0; i < children.length; ++i) {
      let child = children[i]
      if (child instanceof ASTNode || !child.op) continue

      if (operators.includes(child.op)) {
        combineBinaryOperator(node, i)
        --i
      }
    }
  }, true)
}

// The index of each operator is also an enum, which is used in comparison chains to describe which operator is being used
const comparisonOperators = ['<', '<=', '==', '!=', '>=', '>']

// Process "comparison chains", which are sequences of the form 0 <= x < 2. Internally these are transformed into
// "comparison_chain" operators, which have the form comparison_chain(0, 1 (enum comparison), x, 0 (enum comparison), 2). Gross, but
// it's hard to cleanly represent these comparison chains otherwise. You *could* represent them using boolean operations,
// but that duplicates the internal nodes which is inefficient
function processComparisonChains (root: ASTNode) {
  root.applyAll(node => {
    // TODO: process backwards
    const children = node.children

    for (let i = 0; i < children.length; ++i) {
      let child = children[i]
      if (child instanceof ASTNode || !child.op) continue

      if (comparisonOperators.includes(children[i].op)) {
        let comparisonChainFound = false

        // Found a comparison operator token; we now check for whether the tokens +2, +4, etc. ahead of it are also
        // comparison tokens. If so, we emit a comparison chain

        // Index of the last comparison token, plus 2
        let j = i + 2
        for (; j < children.length; j += 2) {
          let nextChild = children[j]
          if (nextChild instanceof ASTNode || !nextChild.op) continue

          if (comparisonOperators.includes(children[j].op)) {
            comparisonChainFound = true
          } else {
            break
          }
        }

        if (comparisonChainFound) {
          // The nodes i, i+2, i+4, ..., j-4, j-2 are all comparison nodes. Thus, all nodes in the range i-1 ... j-1
          // should be included in the comparison chain

          let comparisonChain = new OperatorNode({ name: 'comparison_chain' })

          // Looks something like [ ASTNode, '<', ASTNode, '<=', ASTNode ]
          let removedChildren = children.splice(
            i - 1,
            j - i + 1,
            comparisonChain // inserts the comparison chain as replacement
          )

          // [ ASTNode, ASTNode, ASTNode ]
          let cchainChildren = (comparisonChain.children = [])

          let comparisons = [] // [ '<', '<=' ]
          for (let i = 1; i < removedChildren.length - 2; i += 2) {
            comparisons.push(removedChildren[i].op)
          }

          for (let i = 0; i < removedChildren.length; i += 2) {
            cchainChildren.push(removedChildren[i])
          }

          comparisonChain.extraArgs.comparisons = comparisons

          return
        }
      }
    }
  }, true)
}

// Remove residual commas from the node
function removeCommas (root: ASTNode) {
  root.applyAll(node => {
    let children = node.children
    let i = children.length
    while (i--) {
      if (children[i].type === 'comma') children.splice(i, 1)
    }
  }, true)
}

function verifyCommaSeparation (root: ASTNode, string: string) {
  root.applyAll(node => {
    // Every function with multiple elements in it should have commas separating each argument, with no leading or
    // trailing commas.
    let children = node.children
    let isPlainGroup = (node instanceof ASTGroup) && !(node instanceof OperatorNode)

    let isFunction = node.isFunctionNode()
    if (!isFunction && !isPlainGroup) return

    if (children.length === 0) return // fine. () is the empty tuple
    if (children[0].type === 'comma') raiseParserError(string, children[0], "leading comma in expression")

    // Must have the form "a,b,c"
    let prevChild = null
    for (let i = 0; i < children.length; ++i) {
      let child = children[i]

      if ((!prevChild || prevChild.type === "comma") && child.type === "comma") {
        // child is a token
        raiseParserError(string, child, "spurious comma")
      }

      if (prevChild && prevChild.type !== "comma" && child.type !== "comma") {
        // child is a node
        raiseParserError(string, child.info, "trailing expression")
      }

      prevChild = child
    }

    // TODO tuples
    if (isPlainGroup) {
      if (children.length > 1) raiseParserError(string, children[2].info, "tuples not yet implemented")
    }

  }, true)
}

/**
 * Attach start and end tokens for each group
 * @param root
 */
function attachInformation (root: ASTNode) {
  root.applyAll(node => {
    let info = node.info
    if (!info.token) {
      info.token = info.startToken = node.children[0].info.token
      info.endToken = node.children[node.children.length - 1].info.token
    }
  }, true, true /* children first */)
}

/**
 * Parse a given list of tokens, returning a single ASTNode.
 * Perf: parseString("x^2+y^2+e^-x^2+pow(3,gamma(2401 + complex(2,3)))" took 0.028 ms / iteration as of Mar 14, 2022.
 * @param tokens {any[]}
 * @param string {string} String where tokens ultimately came from (used for descriptive error messages)
 * @returns {ASTNode}
 */
function parseTokens (tokens: Array<Token>, string: string): ASTNode {
  // This is somewhat of a recursive descent parser because the grammar is nontrivial, but really isn't that
  // crazy. At intermediate steps, the node is a tree of both processed nodes and unprocessed tokens—a bit odd, but it
  // works.

  // Placed here because all further nodes will be groups or OperatorNodes
  processConstantsAndVariables(tokens)

  // Everything is done recursively within this root node
  let root = new ASTGroup()
  root.children = tokens

  processParentheses(root)
  processFunctions(root)

  // Order of operations: unary -/+, ^ (all right to left, together); PEMDAS
  processUnaryAndExponentiation(root)
  processOperators(root, ['*', '/'])
  processOperators(root, ['-', '+'])

  // Comparison chains are expressions of the form x <= y < z ..., which are combined into a single funky node called a
  // "comparison_chain" with arguments x, y, z and extra arguments <=, <
  processComparisonChains(root)
  processOperators(root, comparisonOperators)
  processOperators(root, ['and', 'or'])

  // Adds "debugging tokens", aka where each node starts and ends, and the top node
  attachInformation(root)
  verifyCommaSeparation(root, string)
  // processTuples(root, string)
  removeCommas(root)

  let c = root.children[0]
  if (!c) throw new Error("Unknown error")

  c.info.parsedFrom = string

  return c
}

function convertToASTNode(n: UnprocessedASTNode): ASTNode {
  let root = null



  return root
}

function parseString (string: string): ASTNode {
  if (typeof string !== "string") {
    throw new ParserError("parseString expects a string")
  }

  checkParensBalanced(string)

  let tokens = getTokens(string)
  checkValid(tokens, string)

  let parsed: UnprocessedASTNode = parseTokens(tokens, string)

  return convertToASTNode(parsed)
}

export { parseString, getTokens }
