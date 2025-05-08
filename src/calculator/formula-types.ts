// Base interface for all formula types
export interface Formula {
  type: string;
}

// Simple propositions like P, Q, R
export class AtomicFormula implements Formula {
  type = 'atomic';
  constructor(public symbol: string) {}
  
  toString(): string {
    return this.symbol;
  }
}

// Formulas connected by binary operators like AND, OR, IMPLIES
export class BinaryFormula implements Formula {
  type = 'binary';
  constructor(
    public operator: string,  // '∧', '∨', '→', '↔'
    public left: Formula,
    public right: Formula
  ) {}
  
  toString(): string {
    return `(${this.left.toString()} ${this.operator} ${this.right.toString()})`;
  }
}

// Negated formulas (NOT)
export class NegatedFormula implements Formula {
  type = 'negated';
  constructor(public formula: Formula) {}
  
  toString(): string {
    return `¬${this.formula.toString()}`;
  }
}

// We can add these if needed for first-order logic
export class QuantifiedFormula implements Formula {
  type = 'quantified';
  constructor(
    public quantifier: string,  // '∀', '∃'
    public variable: string,
    public formula: Formula
  ) {}
  
  toString(): string {
    return `${this.quantifier}${this.variable}(${this.formula.toString()})`;
  }
}
