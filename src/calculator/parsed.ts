// Base interface for all AST nodes
interface BaseNode {
  type: string;
}

// For simple atomic propositions like p, q
interface AtomicNode extends BaseNode {
  type: "atomic";
  symbol: string;
}

// For predicates like Fxy
interface PredicateNode extends BaseNode {
  type: "predicate";
  symbol: string;            // The predicate name (e.g., F)
  arguments: AtomicNode[]; // The arguments (e.g., x, y)
}

// For quantifiers like ∀x, ∃y
interface QuantifierNode extends BaseNode {
  type: "quantifier";
  symbol: string;      // The quantifier symbol (e.g., ∀, ∃)
  variable: string;    // The variable being quantified
  body: ASTNode;       // The expression being quantified
}

// For logical operators like ∧, ∨, →, etc.
interface OperatorNode extends BaseNode {
  type: "operator";
  symbol: string;
  children: ASTNode[];
}

// Union type for all possible nodes
export type ASTNode = 
  | AtomicNode 
  | PredicateNode 
  | QuantifierNode 
  | OperatorNode;

const result2: ASTNode = {
  type: "operator",
  symbol: "|=",
  children: [
    {
      type: "operator",
      symbol: "∨",
      children: [
        {
          type: "atomic",
          symbol: "q"
        },
        {
          type: "atomic",
          symbol: "q"
        }
      ]
    },
    {
      type: "operator",
      symbol: "~",
      children: [
        {
          type: "atomic",
          symbol: "p"
        }
      ]
    },
    {
      type: "atomic",
      symbol: "q"
    }
  ]
};

