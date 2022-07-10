import { ConcreteType } from "./type.js";

const ScopeTypeInit = () => {
  throw new Error("?")
}

/**
 * "Formal" concrete type representing a scope input into a compiled function; not used in general ASTNodes
 */
export class ScopeType extends ConcreteType {
  scopeDict: [string: ConcreteType]

  constructor(scopeDict) {
    super({
      name: "scope",
      init: ScopeTypeInit,
      castPermissive: ScopeTypeInit
    })

    this.scopeDict = scopeDict
  }
}
