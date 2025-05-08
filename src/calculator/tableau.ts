// ================ Formula Types ================

export interface Proposition {
  type: 'proposition';
  symbol: string;
}

export interface Negation {
  type: 'negation';
  formula: Formula;
}

export interface Conjunction {
  type: 'conjunction';
  left: Formula;
  right: Formula;
}

export interface Disjunction {
  type: 'disjunction';
  left: Formula;
  right: Formula;
}

export interface Implication {
  type: 'implication';
  left: Formula;
  right: Formula;
}

type NegationOf<T extends Formula> = Negation & { formula: T };

export type Formula = Proposition | Negation | Conjunction | Disjunction | Implication;

// ================ Formula Creation Helpers ================

export const prop = (symbol: string): Formula => ({ type: 'proposition', symbol });
export const not = (formula: Formula): Formula => ({ type: 'negation', formula });
export const and = (left: Formula, right: Formula): Formula => ({
  type: 'conjunction',
  left,
  right,
});
export const or = (left: Formula, right: Formula): Formula => ({
  type: 'disjunction',
  left,
  right,
});
export const implies = (left: Formula, right: Formula): Formula => ({
  type: 'implication',
  left,
  right,
});

// ================ Tableau Logic ================

type RuleType = 'alpha' | 'beta' | 'gamma' | 'delta' | 'other';

// Generic rule descriptor
export type FormulaRule<T extends Formula = Formula> = {
  ruleType: RuleType;
  description: string;
  matches: (f: Formula) => f is T;
  getComponents: (f: T) => Formula[];
};

// Collection of all rules
const formulaRules: FormulaRule<any>[] = [
  {
    ruleType: 'alpha',
    description: 'Conjunction: p∧q',
    matches: (f): f is Conjunction => f.type === 'conjunction',
    getComponents: (f) => [f.left, f.right],
  },
  {
    ruleType: 'alpha',
    description: 'Negated disjunction: ¬(p∨q) ↔ ¬p∧¬q',
    matches: (f): f is NegationOf<Disjunction> =>
      f.type === 'negation' && f.formula.type === 'disjunction',
    getComponents: (f) => [not(f.formula.left), not(f.formula.right)],
  },
  {
    ruleType: 'alpha',
    description: 'Negated implication: ¬(p→q) ↔ p∧¬q',
    matches: (f): f is NegationOf<Implication> =>
      f.type === 'negation' && f.formula.type === 'implication',
    getComponents: (f) => [f.formula.left, not(f.formula.right)],
  },
  {
    ruleType: 'alpha',
    description: 'Double negation: ¬¬p ↔ p',
    matches: (f): f is NegationOf<Negation> =>
      f.type === 'negation' && f.formula.type === 'negation',
    getComponents: (f) => [f.formula.formula],
  },
  {
    ruleType: 'beta',
    description: 'Disjunction: p∨q',
    matches: (f): f is Disjunction => f.type === 'disjunction',
    getComponents: (f) => [f.left, f.right],
  },
  {
    ruleType: 'beta',
    description: 'Implication: p→q ↔ ¬p∨q',
    matches: (f): f is Implication => f.type === 'implication',
    getComponents: (f) => [not(f.left), f.right],
  },
  {
    ruleType: 'beta',
    description: 'Negated conjunction: ¬(p∧q) ↔ ¬p∨¬q',
    matches: (f): f is NegationOf<Conjunction> =>
      f.type === 'negation' && f.formula.type === 'conjunction',
    getComponents: (f) => [not(f.formula.left), not(f.formula.right)],
  },
];

// Atomic if it's a proposition or ¬(proposition)
export const isAtomic = (formula: Formula): boolean =>
  formula.type === 'proposition' ||
  (formula.type === 'negation' && formula.formula.type === 'proposition');

// Branch closed if it contains p and ¬p
export const isClosed = (branch: Formula[]): boolean =>
  branch.some(
    (f1) =>
      f1.type === 'proposition' &&
      branch.some(
        (f2) =>
          f2.type === 'negation' &&
          f2.formula.type === 'proposition' &&
          f2.formula.symbol === f1.symbol
      )
  );

// Check rule type
export const isRuleType = (formula: Formula, type: RuleType): boolean =>
  formulaRules.some((rule) => rule.ruleType === type && rule.matches(formula));

// Retrieve components via matching rule
export function getComponents(formula: Formula): Formula[] {
  const rule = formulaRules.find((r) => r.matches(formula));
  return rule ? rule.getComponents(formula as any) : [];
}

// Alpha/Beta checks & specialized getters
export const isAlpha = (f: Formula): boolean => isRuleType(f, 'alpha');
export const isBeta = (f: Formula): boolean => isRuleType(f, 'beta');

export function getAlphaComponents(formula: Formula): Formula[] {
  const rule = formulaRules.find((r) => r.ruleType === 'alpha' && r.matches(formula));
  return rule ? rule.getComponents(formula as any) : [];
}

export function getBetaComponents(formula: Formula): [Formula, Formula] {
  const rule = formulaRules.find((r) => r.ruleType === 'beta' && r.matches(formula));
  if (!rule) throw new Error(`Not a beta formula: ${formulaToString(formula)}`);
  return rule.getComponents(formula as any) as [Formula, Formula];
}

// ================ Tableau Building ================

export function buildTableau(formulas: Formula[]): Formula[][] {
  let tableau: Formula[][] = [formulas];
  let expanded = true;

  while (expanded) {
    expanded = false;
    tableau = tableau.flatMap((branch) => {
      if (isClosed(branch)) return [branch];

      for (let i = 0; i < branch.length; i++) {
        const f = branch[i];
        if (f === undefined) continue;
        if (isAtomic(f)) continue;
        const rule = formulaRules.find((r) => r.matches(f));
        if (!rule) continue;

        expanded = true;
        const rest = branch.filter((_, idx) => idx !== i);
        const comps = rule.getComponents(f as any);

        if (rule.ruleType === 'alpha') {
          return [rest.concat(comps)];
        } else if (rule.ruleType === 'beta') {
          return comps.map((c) => rest.concat([c]));
        }
      }
      return [branch];
    });
  }

  return tableau;
}

// ================ Satisfiability & Validity ================

export function checkSatisfiability(formulas: Formula[]): {
  satisfiable: boolean;
  model?: Record<string, boolean>;
} {
  const tableau = buildTableau(formulas);
  for (const branch of tableau) {
    if (!isClosed(branch)) {
      const model: Record<string, boolean> = {};
      branch.forEach((f) => {
        if (f.type === 'proposition') model[f.symbol] = true;
        if (f.type === 'negation' && f.formula.type === 'proposition')
          model[f.formula.symbol] = false;
      });
      return { satisfiable: true, model };
    }
  }
  return { satisfiable: false };
}

export function checkValidity(premises: Formula[], conclusion: Formula): boolean {
  const { satisfiable } = checkSatisfiability([...premises, not(conclusion)]);
  return !satisfiable;
}

// ================ Utilities & Testing ================

export function formulaToString(formula: Formula): string {
  switch (formula.type) {
    case 'proposition':
      return formula.symbol;
    case 'negation':
      return `¬${formulaToString(formula.formula)}`;
    case 'conjunction':
      return `(${formulaToString(formula.left)} ∧ ${formulaToString(formula.right)})`;
    case 'disjunction':
      return `(${formulaToString(formula.left)} ∨ ${formulaToString(formula.right)})`;
    case 'implication':
      return `(${formulaToString(formula.left)} → ${formulaToString(formula.right)})`;
  }
}

export function printTableau(tableau: Formula[][]): void {
  tableau.forEach((branch, i) => {
    console.log(`Branch ${i + 1}:`);
    branch.forEach((f) => console.log(`- ${formulaToString(f)}`));
    console.log(`Closed: ${isClosed(branch)}`);
  });
}

export function testArgument(name: string, premises: Formula[], conclusion: Formula): void {
  console.log(`Testing validity of ${name}`);
  premises.forEach((f) => console.log(`- ${formulaToString(f)}`));
  console.log(`Conclusion: ${formulaToString(conclusion)}`);
  console.log(`The argument is ${checkValidity(premises, conclusion) ? 'valid' : 'invalid'}.`);
}

// Example usage
export function main(): void {
  const p = prop('p');
  const q = prop('q');
  const r = prop('r');
  testArgument('p∨q, ¬p |= q', [or(p, q), not(p)], q);
  testArgument('p→q, p |= q', [implies(p, q), p], q);
  testArgument('p→q, ¬q |= ¬p', [implies(p, q), not(q)], not(p));
  testArgument('p→q, q→r |= p→r', [implies(p, q), implies(q, r)], implies(p, r));
}

// Run
main();
