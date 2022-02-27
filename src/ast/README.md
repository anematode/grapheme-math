## AST

An [abstract syntax tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree) (AST) is how Grapheme represents expressions. At the time of writing (December 5, 2021), the AST only handles *expressions*, but it can—and hopefully will—be extended to more general things like equations. I also hope it will someday have functions as first-class citizens, and the Grapheme language, if ever created, would likely be mostly functional in design.

I am too lazy and prideful to use an existing parser like [Nearley](https://github.com/kach/nearley) to parse expressions. I am also too lazy to strictly define the grammar. It will almost certainly become more complex over time. An executive summary:

* Unary operators: -
* Binary operators: +, *, /, ^ (exponentiation), <, >, <=, >=, ==, !=, and, or. The last two operators need to be separated by some spacing in most contexts to separate them from, say, function names.
* Precedence: Most operators follow PEMDAS, then the boolean operators, evaluated left-to-right. The exceptions are unary minus and exponentiation, which are processed in one step from right to left. Thus, `e^-x^2` is parsed as `e^(-(x^2))` instead of, say, `(e^(-x))^2`.
* Grouping via parentheses: Parentheses must be balanced. There is no implicit multiplication between two parenthesized expressions.
* Function calls: Functions must begin with a letter or underscore and may otherwise contain alphanumeric or underscore characters, followed by parentheses containing their arguments.
* Constants: Float-like expressions like `0.3`

In the compiled result, there are operators, constants, and variables. Operators include both inline operators and functions and may thus take in any number of operations; *+* is basically treated as a function taking in two variables, with the name "+". Constants are, well, constants, mainly numerical values (but also potentially strings). Variables have the same naming convention as functions, and currently, functions and variables may share names.

## Types

Grapheme is strongly typed. In any given well-formed expression, a variable or intermediate result has a type, deduced from the types of the variables and the available variables. Consider the expression `x^2+y^2`; if both variables have type `int`, then the whole expression and each expression `x^2`, `y^2` also has type `int`. The `^` operation is *resolved* to be the one that has the signature `(int, int) -> int`, as is the `+` operation. Meanwhile, if both have type `real`, the whole expression (and each subexpression) also have type `real`. If `x` is `real` while `y` is `complex`, then `x^2` is `real` while `y^2` is complex, and `x^2` is *implicitly casted* to `complex`, so that the `+` operation is resolved to the one with signature `(complex, complex) -> complex`. Operations are thus fairly overloaded.

Why make things strongly typed? The main reason is performance. The reason Grapheme is so much faster than math.js at evaluating expressions—80x faster in some cases—is because it can compile expressions directly into JavaScript and resolve every relevant function call at compile, rather than having to resolve them during evaluation. The drawback, of course, is that a compiled function like `(x,y) -> x+y` cannot be called with anything other than the types it was compiled with.

The **mathematical types** of expressions are still abstractions over the *underlying type*, or **concrete type**, that will be used. That's because expressions may be evaluated in various modes. Currently the important modes are *normal* and *fast_interval*, but different modes are totally possible: double–double arithmetic, arbitrary-precision arithmetic, tagged intervals, to name a few. Normal evaluation uses JavaScript numbers for all calculations, and fast interval evaluation uses interval arithmetic, with non-strict rounding and JavaScript numbers. For example, a `real` may ordinarily be represented as a JavaScript number, but in fast interval mode may be represented as a `FastRealInterval`.
 
In general, Grapheme considers things at varying levels of abstraction. At the top is the unparsed expression, which cannot be evaluated or even meaningfully manipulated. In the middle, each subexpression has been assigned a (mathematical) type based on the types of each variable and the resolutions of various functions, including implicit casting. At the bottom, an evaluation mode has been selected and each subexpression has been assigned a concrete type. We call the latter two the "mathematical" and "concrete" world, respectively.

The mathematical world is relatively well-behaved and is intended to be separated from issues like overflow. For example, `3^100` should be considered as `^(int, int) -> int`, even though the result isn't representable in a 64-bit float. Having a mathematical intermediate representation allows for not only multiple modes of evaluation but also special handling of certain common expressions. For example, an optimizing evaluator/compiler could recognize the pattern `mod(3^1000, 5)` and evaluate it exactly using [exponentiation by squaring](https://en.wikipedia.org/wiki/Exponentiation_by_squaring). The concrete world is subject to the limitations of whatever representations are chosen. `mod(3^1000, 5)` might return the mathematically correct value `1`, or `NaN`, or, if compiled to do so, throw an overflow error or set some sort of overflow flag. (Point is, the concrete world has a lot more flexibility, including choosing the right balance between performance and convenience.)
 
## Operator resolution

Suppose we have generated an AST `tree` for the expression `x^2+1` with an EvaluationContext `ctx`. The expression has no other information besides the operator names and their order of evaluation; *it has no mathematical type* and *does not know what function to call*. The only parts of the expression with a type are the nodes `1` and `2`, which each have type `int`.

So, we call `tree.resolveTypes({ x: "real" })`, which informs the tree that the variable `x` should be considered a real number. The resolution goes depth-first, first looking at the call to operator `^`. There may be a host of **definitions** in `ctx` for this operator: `^(int, int) -> int`, `^(real, real) -> real`, or even `^(complex, int) -> complex`. If there is a definition `^(real, int) -> real`, then that definition is a perfect match and will be chosen. **No two definitions for an operator should have the same types.** But let's suppose a definition `^(real, int) -> real` doesn't exist, and only the three aforementioned ones do.

### Implicit casting

How is the correct operator selected? The answer is that types have certain implicit *casts* between them. For example, `int -> real`, `real -> complex`, and `int -> complex` are all implicit casts. Casts are just a special type of operator definition. If there is no perfect match for the operator definition, the best one is chosen as follows:

1. The number of required casts shall be minimized.
2. If there are multiple definitions with the same number of casts, the one that was defined first is chosen.

Consider the example again. `^(int, int) -> int` doesn't work at all, since there is no cast `real -> int`. `^(real, real) -> real` has one cast (the second argument). `^(complex, int) -> complex` has one cast (the first argument). Because the second cast comes first, it is selected.

Should the cast be inserted as a new node taking in a single argument and returning the casted result, or should it be stored specially? The benefit of the first method is that it reflects what must actually be computed. The benefit of the second method is that it is more faithful to the original expression, especially because most implicit casts are trivial from a mathematical perspective. The cast is stored in the *parent operator*, not the child, as an array of casts (with a cast for each argument, `null` if no cast is necessary). Finally, the `+` has a type of `real` and the whole expression is a real number.

## Entering the concrete

We have now given the entire expression and each subexpression a type, an operator definition, and a casting array. But we still can't evaluate the function! We need to compile it into a concrete form, giving each subexpression a concrete type. There are several evaluation modes, with the most common being `normal`, using standard double-precision floating-point throughout. The abstract `int` is mapped to the *concrete* `int`, which is distinct and has an actual representation: a standard JS number, restricted to `{ ±inf, NaN, -0 } U { representable integers }`. `complex` is mapped to the concrete `complex`, internally represented by something like `Grapheme.Complex`.

In turn, each operator definition and cast must find a suitable **evaluator**, which is a function that actually does the legwork. For example, `^(real, real) -> real` will be the `Math.pow` function, while `^(int, int) -> int` will probably be some custom function that does fast integer exponentiation by repeated multiplication. Note that each of these evaluators is now taking in *concrete types*. For example, the operator `^(real, real) -> real` may have two evaluators, `^(real, real) -> real` and `^(fast_real_interval, fast_real_interval) -> fast_real_interval`. Some evaluators are trivial, like the cast `real(int) -> real` (note that casts can be explicitly invoked as a function). Some may call a built-in function directly, like `sin(real) -> real` calling `Math.sin`. And most are specialized functions, like `gamma(complex) -> complex`.

## Entering the callable: intermediate representation and optimization

Functions, especially when compiled in normal mode, need to be as fast as possible. The resolution of evaluators at compile time is a significant step, but optimizations remain a bit tricky.

An intermediate representation is basically a series of assignments, with a single special variable "result" used for the output, and a single "if-statement" primitive corresponding to certain boolean constructs like piecewise and short-circuiting operators. This "if" primitive is effectively a specialized ternary operator: It immediately evaluates a single condition, and based on that condition, evaluates some of the other two operands.

It is possible that both operands be evaluated, none be evaluated. Consider the expression `piecewise(sqrt(x+5) < 1, x^2, sqrt(x))`. In normal mode, the condition operand is either true (when x is between -6 and -4), false (when x is greater than -4), or undefined (when x is less than -6). In the first case, `x^2` is evaluated and returned. In the second case, `sqrt(x)` is evaluated and returned, even if it is NaN. In the third case, the condition is undefined, so neither case is evaluated and NaN is returned. In the case of interval arithmetic the situation is even more complex; both operands must be evaluated if the condition is potentially true or false, and their union taken. An executive summary of the `if(cond, op1, op2)` primitive:

| Condition (immediately evaluated) | Operand 1 | Operand 2 | notes |
|-----------------------------------| --- | --- | --- |
| true                              | evaluated | not evaluated | |
| false                             | not evaluated | evaluated | |
| undefined                         | not evaluated | not evaluated | |
| uncertain                         | evaluated | evaluated | union taken |

Of note, the underlying type of the condition is therefore dependent on the evaluation mode.

Consider the aforementioned expression. We get an unoptimized structure of the following form (`<-` denotes assignment):

```
a: int <- 5                                                             (constant)
b: real <- real(a)                                                      (cast)
y: real <- add(x, 5)
z: real <- sqrt(y)
c: boolean <- NullableBoolean.Comparison.less(z, 1)                     (potentially nullable)

result: real <- if (c) {                                                (if construct)
  c: int <- 2
  pow(x, 2)
} else {
  sqrt(x)
}
```

This might roughly compile to the following optimized JS code:

```
function f(x) {
    // optional typechecks elided
    
    var $y = x + 5;
    var $z = Math.sqrt($y);
    if ($z !== $z) { // null check
       return NaN;
    }
    if ($z < 1) {
       return Math.pow(x, 2);
    }
    
    return Math.sqrt(x);
}
```

The only other control flow construct really used is breaking out of labeled blocks, used effectively as a weird jump. Observe:

We avoid most control flow constructs—only if statements and breaking out of blocks, the latter used for global constructs like errors and NaN propagation.

Thus, an intermediate function representation 

Recall that we have various concrete types and their evaluators. 

## Contexts

There is reason to have different evaluation contexts. For example, it'd make sense for a function `f` and variables `a`, `b` to be defined in one context but not accessible from another. It doesn't make sense, however, to have built-in evaluators be unique to each context; that would be highly inefficient.

## Summary

Grapheme is strongly typed. **Expressions**, or **abstract syntax trees**, have no intrinsic types until they are resolved by giving the types of the variables in it. In turn, **operator definitions** are resolved, so that the whole expression and each of its subexpressions has an operator definition, a **mathematical type**, and potentially an array of **implicit casts**.

An expression whose types have been resolved may then be compiled. A mode is selected, giving a mapping between mathematical types and **concrete types**—the underlying types that will be used in computation. Each operator definition and cast is mapped to an **evaluator**, which has takes in concrete types as arguments and outputs a concrete type.

The mapping between these concepts and actual Grapheme classes is as follows (`<-` denotes inheritance):

| Concept | Class | Inhabits |
| --- | --- | --- |
| Context | EvaluatorContext | *mahematical and concrete world* |
| AST | `ASTNode, ASTNode <- OperatorNode, VariableNode, ConstantNode` | *pre-mathematical world* before type resolution, *mathematical world* after |
| Operator definition | `OperatorDefinition` | *mathematical world* |
| Mathematical type | `MathematicalType` | *mathematical world* |
| Implicit cast | `OperatorDefinition <- Cast` | *mathematical world* |
| Concrete type | `ConcreteType` | *concrete world* |
| Evaluator | `EvaluatorDefinition` | *concrete world* |

Currently types are fairly simple, but in the future they may include generics—something like `list<int>`, for example.

### Concrete types

#### bool

Takes on one of `0`, `1`, `NaN`, with `0` meaning "false", `1` meaning true, and `NaN` being the null value (undefined).

#### int

Integer between `-2 ** 53 = -9007199254740992` and `2 ** 53 - 1 = 9007199254740991`
