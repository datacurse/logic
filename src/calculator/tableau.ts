// ================ Formula Types ===================

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

export type Formula = Proposition | Negation | Conjunction | Disjunction | Implication;

type NegatedDisjunction = Negation & { formula: Disjunction };
type NegatedImplication = Negation & { formula: Implication };
type DoubleNegation = Negation & { formula: Negation };
type NegatedConjunction = Negation & { formula: Conjunction };

export const isNegatedDisjunction = (f: Formula): f is NegatedDisjunction =>
  isNegation(f) && isDisjunction(f.formula);

export const isNegatedImplication = (f: Formula): f is NegatedImplication =>
  isNegation(f) && isImplication(f.formula);

export const isDoubleNegation = (f: Formula): f is DoubleNegation =>
  isNegation(f) && isNegation(f.formula);

export const isNegatedConjunction = (f: Formula): f is NegatedConjunction =>
  isNegation(f) && isConjunction(f.formula);

// ================ Type Guards ===================

export const isProposition = (f: Formula): f is Proposition => f.type === 'proposition';
export const isNegation = (f: Formula): f is Negation => f.type === 'negation';
export const isConjunction = (f: Formula): f is Conjunction => f.type === 'conjunction';
export const isDisjunction = (f: Formula): f is Disjunction => f.type === 'disjunction';
export const isImplication = (f: Formula): f is Implication => f.type === 'implication';

// ================ Formula Creation Helpers ===================

export const prop = (symbol: string): Proposition => ({ type: 'proposition', symbol });
export const not = (formula: Formula): Negation => ({ type: 'negation', formula });
export const and = (left: Formula, right: Formula): Conjunction => ({
  type: 'conjunction',
  left,
  right,
});
export const or = (left: Formula, right: Formula): Disjunction => ({
  type: 'disjunction',
  left,
  right,
});
export const implies = (left: Formula, right: Formula): Implication => ({
  type: 'implication',
  left,
  right,
});

// ================ Rule Definitions ===================

type RuleType = 'alpha' | 'beta';

export interface FormulaRule {
  ruleType: RuleType;
  description: string;
  matches(f: Formula): boolean;
  getComponents(f: Formula): Formula[];
}

const formulaRules: FormulaRule[] = [
  {
    ruleType: 'alpha',
    description: 'Conjunction: p ∧ q',
    matches: isConjunction,
    getComponents: (f: Conjunction) => [f.left, f.right],
  },
  {
    ruleType: 'alpha',
    description: 'Negated disjunction: ¬(p ∨ q) ↔ ¬p ∧ ¬q',
    matches: isNegatedDisjunction,
    getComponents: (f: NegatedDisjunction) => [not(f.formula.left), not(f.formula.right)],
  },
  {
    ruleType: 'alpha',
    description: 'Negated implication: ¬(p → q) ↔ p ∧ ¬q',
    matches: isNegatedImplication,
    getComponents: (f: NegatedImplication) => [f.formula.left, not(f.formula.right)],
  },
  {
    ruleType: 'alpha',
    description: 'Double negation: ¬¬p ↔ p',
    matches: isDoubleNegation,
    getComponents: (f: DoubleNegation) => [f.formula.formula],
  },
  {
    ruleType: 'beta',
    description: 'Disjunction: p ∨ q',
    matches: isDisjunction,
    getComponents: (f: Disjunction) => [f.left, f.right],
  },
  {
    ruleType: 'beta',
    description: 'Implication: p → q ↔ ¬p ∨ q',
    matches: isImplication,
    getComponents: (f: Implication) => [not(f.left), f.right],
  },
  {
    ruleType: 'beta',
    description: 'Negated conjunction: ¬(p ∧ q) ↔ ¬p ∨ ¬q',
    matches: isNegatedConjunction,
    getComponents: (f: NegatedConjunction) => [not(f.formula.left), not(f.formula.right)],
  },
];

// ================ Utilities ===================

export const isAtomic = (f: Formula): boolean =>
  isProposition(f) || (isNegation(f) && isProposition(f.formula));

export const isClosed = (branch: Formula[]): boolean =>
  branch.some(
    (f1) =>
      isProposition(f1) &&
      branch.some(
        (f2) => isNegation(f2) && isProposition(f2.formula) && f2.formula.symbol === f1.symbol
      )
  );

function findRule(f: Formula): FormulaRule | undefined {
  return formulaRules.find((r) => r.matches(f));
}

export const isAlpha = (f: Formula): boolean => findRule(f)?.ruleType === 'alpha';
export const isBeta = (f: Formula): boolean => findRule(f)?.ruleType === 'beta';

export function getAlphaComponents(f: Formula): Formula[] {
  const rule = findRule(f);
  return rule && rule.ruleType === 'alpha' ? rule.getComponents(f) : [];
}

export function getBetaComponents(f: Formula): [Formula, Formula] {
  const rule = findRule(f);
  if (!rule || rule.ruleType !== 'beta')
    throw new Error(`Not a beta formula: ${formulaToString(f)}`);
  return rule.getComponents(f) as [Formula, Formula];
}

export function getComponents(f: Formula): Formula[] {
  const rule = findRule(f);
  return rule ? rule.getComponents(f) : [];
}

// ================ Tableau Construction ===================

export function buildTableau(premises: Formula[]): Formula[][] {
  let tableau: Formula[][] = [premises];
  let expanded: boolean;

  do {
    expanded = false;
    const next: Formula[][] = [];

    for (const branch of tableau) {
      if (isClosed(branch)) {
        next.push(branch);
        continue;
      }

      let applied = false;
      for (const f of branch) {
        if (!f || isAtomic(f)) continue;
        const rule = findRule(f);
        if (!rule) continue;

        const rest = branch.filter((x) => x !== f);
        const comps = rule.getComponents(f);

        if (rule.ruleType === 'alpha') {
          next.push([...rest, ...comps]);
        } else {
          for (const c of comps) {
            next.push([...rest, c]);
          }
        }

        expanded = true;
        applied = true;
        break;
      }

      if (!applied) next.push(branch);
    }

    tableau = next;
  } while (expanded);

  return tableau;
}

// ================ Satisfiability & Validity ===================

export function checkSatisfiability(formulas: Formula[]): {
  satisfiable: boolean;
  model?: Record<string, boolean>;
} {
  const tableau = buildTableau(formulas);

  for (const branch of tableau) {
    if (!isClosed(branch)) {
      const model: Record<string, boolean> = {};
      for (const f of branch) {
        if (isProposition(f)) model[f.symbol] = true;
        else if (isNegation(f) && isProposition(f.formula)) model[f.formula.symbol] = false;
      }
      return { satisfiable: true, model };
    }
  }

  return { satisfiable: false };
}

export function checkValidity(premises: Formula[], conclusion: Formula): boolean {
  return !checkSatisfiability([...premises, not(conclusion)]).satisfiable;
}

// ================ Utilities & Testing ===================

export function formulaToString(f: Formula): string {
  switch (f.type) {
    case 'proposition':
      return f.symbol;
    case 'negation':
      return `¬${formulaToString(f.formula)}`;
    case 'conjunction':
      return `(${formulaToString(f.left)} ∧ ${formulaToString(f.right)})`;
    case 'disjunction':
      return `(${formulaToString(f.left)} ∨ ${formulaToString(f.right)})`;
    case 'implication':
      return `(${formulaToString(f.left)} → ${formulaToString(f.right)})`;
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
  console.log(`Testing ${name}`);
  premises.forEach((f) => console.log(`- ${formulaToString(f)}`));
  console.log(`Conclusion: ${formulaToString(conclusion)}`);
  console.log(`Valid? ${checkValidity(premises, conclusion)}`);
}

export function main(): void {
  const p = prop('p');
  const q = prop('q');
  const r = prop('r');
  testArgument('p∨q, ¬p ⊢ q', [or(p, q), not(p)], q);
  testArgument('p→q, p ⊢ q', [implies(p, q), p], q);
  testArgument('p→q, ¬q ⊢ ¬p', [implies(p, q), not(q)], not(p));
  testArgument('p→q, q→r ⊢ p→r', [implies(p, q), implies(q, r)], implies(p, r));
}

main();
