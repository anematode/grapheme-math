import { KEYWORD_OPERATOR_NAMES } from "./parse_expression.js";

export function isKeywordOperator(s: string): boolean {
  return KEYWORD_OPERATOR_NAMES.includes(s as any)
}

const PRECEDENCES = {
  '': 20,
  'cchain': 19,
  '^': 11,
  // unary minus is handled separately
  '*': 10,
  '/': 10,
  '+': 8,
  '-': 8,
}

export function getOperatorPrecedence(s: string, arity: number): number {
  // PFCUEMDAS, where the empty string is considered to be generic parentheses. P=F, U=E right to left, M=D, A=S. If the
  // returned precedence is odd, it should be considered right to left.
  if (arity !== 1 && arity !== 2) throw new RangeError("?")

  if (arity === 1) {
    return (s === '-') ? 11 : -2
  }

  let p: number | undefined = PRECEDENCES[s]

  return p ?? 20
}
