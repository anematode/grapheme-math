
describe("parseString", () => {
  it("Correctly parses various expressions", () => {
    // TODO provide hard test cases, esp with weird ones like "and" and "or"
    const testCases = [
      "x^2", `OperatorNode{name="^", children=List{VariableNode{name="x"}, ConstantNode{type=int, value=2}}}`,
      "a+b+c", `OperatorNode{name="+", children=List{OperatorNode{name="+", children=List{VariableNode{name="a"}, VariableNode{name="b"}}}, VariableNode{name="c"}}}`, // left to right

    ]
  })

  it("Correctly throws on malformed expressions", () => {
    const testCases = [
      "x^", "",
    ]
  })
})
