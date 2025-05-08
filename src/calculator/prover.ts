import { Formula, AtomicFormula, BinaryFormula, NegatedFormula } from './formula-types';
import { Parser } from './parser';

/**
 * Tableau rule interface
 */
export interface Rule {
  name: string;
  priority: number;
  match(node: Node, branch: Branch): boolean;
  apply(branch: Branch, node: Node, prover: Prover): Branch[];
}

/** A node in the tableau */
export class Node {
  constructor(public formula: Formula) {}
}

/** A branch (one path) in the tableau */
export class Branch {
  nodes: Node[] = [];
  closed = false;

  constructor(formulas: Formula[]) {
    this.nodes = formulas.map(f => new Node(f));
  }

  /** Find next non-literal node */
  nextNode(): Node | null {
    return this.nodes.find(n => !this.isLiteral(n)) || null;
  }

  isLiteral(n: Node): boolean {
    // Only atomic or negated atomic count as literals
    if (n.formula instanceof AtomicFormula) return true;
    if (n.formula instanceof NegatedFormula) {
      return (n.formula as NegatedFormula).formula instanceof AtomicFormula;
    }
    return false;
  }

  addNode(n: Node) {
    const repr = n.formula.toString();
    if (!this.nodes.some(x => x.formula.toString() === repr)) {
      this.nodes.push(n);
    }
  }

  close() { this.closed = true; }
}

/** The tableau prover */
export class Prover {
  branches: Branch[] = [];

  constructor(
    initial: Formula[],
    private parser: Parser,
    private rules: Rule[]
  ) {
    this.branches.push(new Branch(initial));
  }

  addBranch(b: Branch) {
    this.branches.push(b);
  }

  private checkClosure(branch: Branch): boolean {
    for (let i = 0; i < branch.nodes.length; i++) {
      const n1 = branch.nodes[i];
      for (let j = i + 1; j < branch.nodes.length; j++) {
        const n2 = branch.nodes[j];
        // p and ¬p
        if (
          n1.formula instanceof AtomicFormula &&
          n2.formula instanceof NegatedFormula &&
          (n2.formula as NegatedFormula).formula.toString() === n1.formula.toString()
        ) return true;
        if (
          n2.formula instanceof AtomicFormula &&
          n1.formula instanceof NegatedFormula &&
          (n1.formula as NegatedFormula).formula.toString() === n2.formula.toString()
        ) return true;
      }
    }
    return false;
  }

  /** One expansion step; false if no open branches remain */
  step(): boolean {
    const branch = this.branches.find(b => !b.closed);
    if (!branch) return false;

    if (this.checkClosure(branch)) {
      branch.close();
      return true;
    }

    const node = branch.nextNode();
    if (!node) return true;

    const rule = this.rules
      .slice().sort((a, b) => a.priority - b.priority)
      .find(r => r.match(node, branch));

    if (rule) {
      const newBranches = rule.apply(branch, node, this) || [];
      newBranches.forEach(b => this.addBranch(b));
    }
    // else no rule → leave branch open

    return true;
  }
}

/** Alpha rule: A ∧ B ⇒ add A and B */
export const AlphaRule: Rule = {
  name: 'alpha',
  priority: 1,
  match(node) {
    return node.formula instanceof BinaryFormula && node.formula.operator === '∧';
  },
  apply(branch, node) {
    const f = node.formula as BinaryFormula;
    branch.addNode(new Node(f.left));
    branch.addNode(new Node(f.right));
    return [];
  }
};

/** Negated implication: ¬(A → B) ⇒ add A, ¬B */
export const NegImpRule: Rule = {
  name: 'neg-imp',
  priority: 1,
  match(node) {
    if (node.formula instanceof NegatedFormula) {
      const inner = (node.formula as NegatedFormula).formula;
      return inner instanceof BinaryFormula && inner.operator === '→';
    }
    return false;
  },
  apply(branch, node) {
    const inner = (node.formula as NegatedFormula).formula as BinaryFormula;
    branch.addNode(new Node(inner.left));
    branch.addNode(new Node(new NegatedFormula(inner.right)));
    return [];
  }
};

/** Implication: A → B ⇒ split into ¬A | B */
export const ImpRule: Rule = {
  name: 'imp',
  priority: 2,
  match(node) {
    return node.formula instanceof BinaryFormula && node.formula.operator === '→';
  },
  apply(branch, node) {
    const f = node.formula as BinaryFormula;
    branch.addNode(new Node(new NegatedFormula(f.left)));
    const copy = branch.nodes.map(n => n.formula);
    const nb = new Branch(copy);
    nb.addNode(new Node(f.right));
    return [nb];
  }
};

/** Negated conjunction: ¬(A ∧ B) ⇒ split into ¬A | ¬B */
export const NegConjRule: Rule = {
  name: 'neg-conj',
  priority: 2,
  match(node) {
    if (node.formula instanceof NegatedFormula) {
      const inner = (node.formula as NegatedFormula).formula;
      return inner instanceof BinaryFormula && inner.operator === '∧';
    }
    return false;
  },
  apply(branch, node) {
    const inner = (node.formula as NegatedFormula).formula as BinaryFormula;
    branch.addNode(new Node(new NegatedFormula(inner.left)));
    const copy = branch.nodes.map(n => n.formula);
    const nb = new Branch(copy);
    nb.addNode(new Node(new NegatedFormula(inner.right)));
    return [nb];
  }
};

/** Disjunction: A ∨ B ⇒ split into A | B */
export const BetaRule: Rule = {
  name: 'beta',
  priority: 3,
  match(node) {
    return node.formula instanceof BinaryFormula && node.formula.operator === '∨';
  },
  apply(branch, node) {
    const f = node.formula as BinaryFormula;
    branch.addNode(new Node(f.left));
    const copy = branch.nodes.map(n => n.formula);
    const nb = new Branch(copy);
    nb.addNode(new Node(f.right));
    return [nb];
  }
};

/** Literal closure: p and ¬p ⇒ close */
export const LiteralRule: Rule = {
  name: 'literal',
  priority: 0,
  match(node, branch) {
    if (node.formula instanceof AtomicFormula) {
      return branch.nodes.some(n =>
        n.formula instanceof NegatedFormula &&
        (n.formula as NegatedFormula).formula.toString() === node.formula.toString()
      );
    }
    if (node.formula instanceof NegatedFormula &&
        (node.formula as NegatedFormula).formula instanceof AtomicFormula) {
      const atom = (node.formula as NegatedFormula).formula;
      return branch.nodes.some(n =>
        n.formula instanceof AtomicFormula &&
        n.formula.toString() === atom.toString()
      );
    }
    return false;
  },
  apply(branch) {
    branch.close();
    return [];
  }
};

