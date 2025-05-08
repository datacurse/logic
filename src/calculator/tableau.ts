// ================ Formula Types ===================

interface Proposition {
  type: 'proposition';
  symbol: string;
}

interface Negation {
  type: 'negation';
  formula: Formula;
}

interface Conjunction {
  type: 'conjunction';
  left: Formula;
  right: Formula;
}

interface Disjunction {
  type: 'disjunction';
  left: Formula;
  right: Formula;
}

interface Implication {
  type: 'implication';
  left: Formula;
  right: Formula;
}

interface Biconditional {
  type: 'biconditional';
  left: Formula;
  right: Formula;
}

// First-order logic types
interface Term {
  type: 'term';
  symbol: string;
}

interface Predicate {
  type: 'predicate';
  symbol: string;
  terms: Term[];
}

interface Universal {
  type: 'universal';
  variable: string;
  formula: Formula;
}

interface Existential {
  type: 'existential';
  variable: string;
  formula: Formula;
}

type Formula =
  | Proposition
  | Negation
  | Conjunction
  | Disjunction
  | Implication
  | Biconditional
  | Predicate
  | Universal
  | Existential;

// Special formula types for type guards
type NegatedDisjunction = Negation & { formula: Disjunction };
type NegatedImplication = Negation & { formula: Implication };
type DoubleNegation = Negation & { formula: Negation };
type NegatedConjunction = Negation & { formula: Conjunction };
type NegatedUniversal = Negation & { formula: Universal };
type NegatedExistential = Negation & { formula: Existential };
type NegatedBiconditional = Negation & { formula: Biconditional };

// ================ Type Guards ===================

function createTypeGuard<T extends Formula | Term>(type: T['type']): (f: Formula | Term) => f is T {
  return (f: Formula | Term): f is T => f != null && typeof f === 'object' && f.type === type;
}

function createNegatedTypeGuard<T extends Formula>(
  isInnerType: (f: Formula) => f is T
): (f: Formula) => f is Negation & { formula: T } {
  return (f: Formula): f is Negation & { formula: T } => isNegation(f) && isInnerType(f.formula);
}

const isProposition = createTypeGuard<Proposition>('proposition');
const isNegation = createTypeGuard<Negation>('negation');
const isConjunction = createTypeGuard<Conjunction>('conjunction');
const isDisjunction = createTypeGuard<Disjunction>('disjunction');
const isImplication = createTypeGuard<Implication>('implication');
const isBiconditional = createTypeGuard<Biconditional>('biconditional');
const isPredicate = createTypeGuard<Predicate>('predicate');
const isUniversal = createTypeGuard<Universal>('universal');
const isExistential = createTypeGuard<Existential>('existential');
const isTerm = createTypeGuard<Term>('term');

const isDoubleNegation = createNegatedTypeGuard<Negation>(isNegation);
const isNegatedConjunction = createNegatedTypeGuard<Conjunction>(isConjunction);
const isNegatedDisjunction = createNegatedTypeGuard<Disjunction>(isDisjunction);
const isNegatedImplication = createNegatedTypeGuard<Implication>(isImplication);
const isNegatedBiconditional = createNegatedTypeGuard<Biconditional>(isBiconditional);
const isNegatedUniversal = createNegatedTypeGuard<Universal>(isUniversal);
const isNegatedExistential = createNegatedTypeGuard<Existential>(isExistential);

// ================ Formula Creation Helpers ===================

const prop = (symbol: string): Proposition => ({ type: 'proposition', symbol });
const not = (formula: Formula): Negation => ({ type: 'negation', formula });
const and = (left: Formula, right: Formula): Conjunction => ({
  type: 'conjunction',
  left,
  right,
});
const or = (left: Formula, right: Formula): Disjunction => ({
  type: 'disjunction',
  left,
  right,
});
const implies = (left: Formula, right: Formula): Implication => ({
  type: 'implication',
  left,
  right,
});
const iff = (left: Formula, right: Formula): Biconditional => ({
  type: 'biconditional',
  left,
  right,
});

// First-order logic helpers
const term = (symbol: string): Term => ({ type: 'term', symbol });
const predicate = (symbol: string, terms: Term[]): Predicate => ({
  type: 'predicate',
  symbol,
  terms,
});
const forAll = (variable: string, formula: Formula): Universal => ({
  type: 'universal',
  variable,
  formula,
});
const exists = (variable: string, formula: Formula): Existential => ({
  type: 'existential',
  variable,
  formula,
});

// ================ Term Utilities ===================

function extractTerms(formulas: Formula[]): Term[] {
  const symbols = new Set<string>();
  const result: Term[] = [];

  function collect(formula: Formula): void {
    if (isPredicate(formula)) {
      formula.terms.forEach((t) => {
        if (!symbols.has(t.symbol)) {
          symbols.add(t.symbol);
          result.push(t);
        }
      });
    } else if (isNegation(formula)) {
      collect(formula.formula);
    } else if (
      isConjunction(formula) ||
      isDisjunction(formula) ||
      isImplication(formula) ||
      isBiconditional(formula)
    ) {
      collect(formula.left);
      collect(formula.right);
    } else if (isUniversal(formula) || isExistential(formula)) {
      collect(formula.formula);
    }
    // Propositions have no terms to extract
  }

  formulas.forEach(collect);
  return result;
}

function freshConstant(existingTerms: Term[]): Term {
  const symbols = new Set(existingTerms.map((t) => t.symbol));
  let counter = 1;
  let symbol = `c${counter}`;

  while (symbols.has(symbol)) {
    counter++;
    symbol = `c${counter}`;
  }

  return term(symbol);
}

// ================ Substitution ===================

function substitute(formula: Formula, variable: string, termToSubstitute: Term): Formula {
  if (isPredicate(formula)) {
    const newTerms = formula.terms.map((t) => (t.symbol === variable ? termToSubstitute : t));
    return predicate(formula.symbol, newTerms);
  } else if (isProposition(formula)) {
    return formula;
  } else if (isNegation(formula)) {
    return not(substitute(formula.formula, variable, termToSubstitute));
  } else if (isConjunction(formula)) {
    return and(
      substitute(formula.left, variable, termToSubstitute),
      substitute(formula.right, variable, termToSubstitute)
    );
  } else if (isDisjunction(formula)) {
    return or(
      substitute(formula.left, variable, termToSubstitute),
      substitute(formula.right, variable, termToSubstitute)
    );
  } else if (isImplication(formula)) {
    return implies(
      substitute(formula.left, variable, termToSubstitute),
      substitute(formula.right, variable, termToSubstitute)
    );
  } else if (isBiconditional(formula)) {
    return iff(
      substitute(formula.left, variable, termToSubstitute),
      substitute(formula.right, variable, termToSubstitute)
    );
  } else if (isUniversal(formula)) {
    if (formula.variable === variable) return formula; // The variable is bound by this quantifier
    return forAll(formula.variable, substitute(formula.formula, variable, termToSubstitute));
  } else if (isExistential(formula)) {
    if (formula.variable === variable) return formula; // The variable is bound by this quantifier
    return exists(formula.variable, substitute(formula.formula, variable, termToSubstitute));
  }
  // If somehow a type not handled reaches here, return it as is (shouldn't happen with `Formula` type)
  return formula;
}

function formulaEquals(f1: Formula, f2: Formula): boolean {
  // This is a structural equality check based on the string representation
  // A more robust check would traverse the structure recursively.
  // For tableau purposes, string equality is usually sufficient if string conversion is canonical.
  return formulaToString(f1) === formulaToString(f2);
}

// ================ Rule Definitions ===================

type RuleType = 'alpha' | 'beta' | 'gamma' | 'delta';

interface TableauContext {
  terms: Term[];
}

interface FormulaRule {
  ruleType: RuleType;
  description: string;
  matches(f: Formula): boolean;
  getComponents(f: Formula, context?: TableauContext): Formula[] | [Formula, Formula];
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
    ruleType: 'alpha',
    description: 'Biconditional: A ↔ B ≡ (A → B) ∧ (B → A)',
    matches: isBiconditional,
    getComponents: (f: Biconditional) => [implies(f.left, f.right), implies(f.right, f.left)],
  },
  {
    ruleType: 'alpha',
    description: 'Negated universal: ¬∀x P(x) ↔ ∃x ¬P(x)',
    matches: isNegatedUniversal,
    getComponents: (f: NegatedUniversal) => [exists(f.formula.variable, not(f.formula.formula))],
  },
  {
    ruleType: 'alpha',
    description: 'Negated existential: ¬∃x P(x) ↔ ∀x ¬P(x)',
    matches: isNegatedExistential,
    getComponents: (f: NegatedExistential) => [forAll(f.formula.variable, not(f.formula.formula))],
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
  {
    ruleType: 'beta',
    description: 'Negated Biconditional: ¬(A ↔ B) ≡ (A ∧ ¬B) ∨ (¬A ∧ B)',
    matches: isNegatedBiconditional,
    getComponents: (f: NegatedBiconditional) => [
      and(f.formula.left, not(f.formula.right)),
      and(not(f.formula.left), f.formula.right),
    ],
  },
  {
    ruleType: 'gamma',
    description: 'Universal quantifier: ∀x P(x)',
    matches: isUniversal,
    getComponents: (f: Universal, context?: TableauContext) => {
      const existingTerms = context?.terms ?? [];
      // If there are no terms yet, introduce a fresh constant
      if (existingTerms.length === 0) {
        const newTerm = freshConstant([]);
        return [substitute(f.formula, f.variable, newTerm)];
      }
      // Otherwise, instantiate with all existing terms
      return existingTerms.map((t) => substitute(f.formula, f.variable, t));
    },
  },
  {
    ruleType: 'delta',
    description: 'Existential quantifier: ∃x P(x)',
    matches: isExistential,
    getComponents: (f: Existential, context?: TableauContext) => {
      const existingTerms = context?.terms ?? [];
      const freshTerm = freshConstant(existingTerms);
      return [substitute(f.formula, f.variable, freshTerm)];
    },
  },
];

// ================ Utilities ===================

const isAtomic = (f: Formula): boolean =>
  isProposition(f) ||
  isPredicate(f) ||
  (isNegation(f) && (isProposition(f.formula) || isPredicate(f.formula)));

function isClosed(branch: Formula[]): boolean {
  const seenFormulas = new Set<string>();
  // Collect atomic formulas and their negations
  const atomicAndNegations = branch.filter(isAtomic);

  for (const f of atomicAndNegations) {
    const f_str = formulaToString(f);
    if (isNegation(f)) {
      // Check if the positive form exists in the branch
      const positiveForm_str = formulaToString(f.formula);
      if (seenFormulas.has(positiveForm_str)) {
        return true; // Contradiction found
      }
    } else {
      // Check if the negated form exists in the branch
      const negatedForm_str = formulaToString(not(f));
      if (seenFormulas.has(negatedForm_str)) {
        return true; // Contradiction found
      }
    }
    seenFormulas.add(f_str);
  }
  return false; // No contradiction found among atomic formulas
}

function findRule(f: Formula): FormulaRule | undefined {
  return formulaRules.find((r) => r.matches(f));
}

const isAlpha = (f: Formula): boolean => findRule(f)?.ruleType === 'alpha';
const isBeta = (f: Formula): boolean => findRule(f)?.ruleType === 'beta';
const isGamma = (f: Formula): boolean => findRule(f)?.ruleType === 'gamma';
const isDelta = (f: Formula): boolean => findRule(f)?.ruleType === 'delta';

function getComponentsForRule(
  f: Formula,
  ruleType: RuleType,
  context?: TableauContext
): Formula[] | [Formula, Formula] {
  const rule = formulaRules.find((r) => r.matches(f) && r.ruleType === ruleType);
  if (!rule) {
    throw new Error(`No ${ruleType} rule found for formula: ${formulaToString(f)}`);
  }
  return rule.getComponents(f, context);
}

export function getAlphaComponents(f: Formula): Formula[] {
  return getComponentsForRule(f, 'alpha') as Formula[];
}

export function getBetaComponents(f: Formula): [Formula, Formula] {
  return getComponentsForRule(f, 'beta') as [Formula, Formula];
}

export function getGammaComponents(f: Formula, terms: Term[]): Formula[] {
  return getComponentsForRule(f, 'gamma', { terms }) as Formula[];
}

export function getDeltaComponents(f: Formula, terms: Term[]): Formula[] {
  return getComponentsForRule(f, 'delta', { terms }) as Formula[];
}

export function getComponents(
  f: Formula,
  context?: TableauContext
): Formula[] | [Formula, Formula] | undefined {
  const rule = findRule(f);
  return rule ? rule.getComponents(f, context) : undefined;
}

// ================ Tableau Construction ===================

interface Branch {
  formulas: Formula[];
  gammaApplied: Set<string>;
  deltaApplied: Set<string>;
  betaApplied: Set<string>;
}

export function buildTableau(premises: Formula[]): Formula[][] {
  let tableau: Branch[] = [
    {
      formulas: premises,
      gammaApplied: new Set(),
      deltaApplied: new Set(),
      betaApplied: new Set(),
    },
  ];
  let expanded = true;

  while (expanded) {
    expanded = false;
    const nextTableau: Branch[] = [];

    for (const branch of tableau) {
      if (isClosed(branch.formulas)) {
        nextTableau.push(branch);
        continue;
      }

      let branchExpanded = false;

      // Apply alpha rules
      for (const formula of branch.formulas) {
        if (isAlpha(formula)) {
          const components = getAlphaComponents(formula);
          const newFormulas = components.filter(
            (comp) => !branch.formulas.some((f) => formulaEquals(f, comp))
          );
          if (newFormulas.length > 0) {
            nextTableau.push({
              formulas: [...branch.formulas, ...newFormulas],
              gammaApplied: new Set(branch.gammaApplied),
              deltaApplied: new Set(branch.deltaApplied),
              betaApplied: new Set(branch.betaApplied),
            });
            branchExpanded = true;
            expanded = true;
            break; // Move to the next branch after applying a rule
          }
        }
      }
      if (branchExpanded) continue;

      // Apply delta rules
      for (const formula of branch.formulas) {
        if (isExistential(formula)) {
          const formulaString = formulaToString(formula);
          if (!branch.deltaApplied.has(formulaString)) {
            const terms = extractTerms(branch.formulas);
            const freshTerm = freshConstant(terms);
            const instantiated = substitute(formula.formula, formula.variable, freshTerm);
            nextTableau.push({
              formulas: [...branch.formulas, instantiated],
              gammaApplied: new Set(branch.gammaApplied),
              deltaApplied: new Set([...branch.deltaApplied, formulaString]),
              betaApplied: new Set(branch.betaApplied),
            });
            branchExpanded = true;
            expanded = true;
            break; // Move to the next branch
          }
        }
      }
      if (branchExpanded) continue;

      // Apply gamma rules
      for (const formula of branch.formulas) {
        if (isUniversal(formula)) {
          const terms = extractTerms(branch.formulas);
          if (terms.length > 0) {
            for (const term of terms) {
              const instantiated = substitute(formula.formula, formula.variable, term);
              const key = `${formulaToString(formula)}_${formulaToString(instantiated)}`;
              if (
                !branch.gammaApplied.has(key) &&
                !branch.formulas.some((f) => formulaEquals(f, instantiated))
              ) {
                nextTableau.push({
                  formulas: [...branch.formulas, instantiated],
                  gammaApplied: new Set([...branch.gammaApplied, key]),
                  deltaApplied: new Set(branch.deltaApplied),
                  betaApplied: new Set(branch.betaApplied),
                });
                branchExpanded = true;
                expanded = true;
                break; // Apply one instantiation at a time for simplicity
              }
            }
            if (branchExpanded) break;
          } else {
            // If no terms exist, instantiate with a fresh constant once
            const freshTerm = freshConstant([]);
            const instantiated = substitute(formula.formula, formula.variable, freshTerm);
            const key = `${formulaToString(formula)}_${formulaToString(instantiated)}`;
            if (
              !branch.gammaApplied.has(key) &&
              !branch.formulas.some((f) => formulaEquals(f, instantiated))
            ) {
              nextTableau.push({
                formulas: [...branch.formulas, instantiated],
                gammaApplied: new Set([...branch.gammaApplied, key]),
                deltaApplied: new Set(branch.deltaApplied),
                betaApplied: new Set(branch.betaApplied),
              });
              branchExpanded = true;
              expanded = true;
              break;
            }
          }
        }
      }
      if (branchExpanded) continue;

      // Apply beta rules (split the branch)
      for (const formula of branch.formulas) {
        if (isBeta(formula)) {
          const formulaString = formulaToString(formula);
          if (!branch.betaApplied.has(formulaString)) {
            const [comp1, comp2] = getBetaComponents(formula);
            nextTableau.push({
              formulas: [...branch.formulas, comp1],
              gammaApplied: new Set(branch.gammaApplied),
              deltaApplied: new Set(branch.deltaApplied),
              betaApplied: new Set([...branch.betaApplied, formulaString]),
            });
            nextTableau.push({
              formulas: [...branch.formulas, comp2],
              gammaApplied: new Set(branch.gammaApplied),
              deltaApplied: new Set(branch.deltaApplied),
              betaApplied: new Set([...branch.betaApplied, formulaString]),
            });
            branchExpanded = true;
            expanded = true;
            break; // Only apply one beta rule per iteration for simplicity
          }
        }
      }

      if (!branchExpanded) {
        nextTableau.push(branch); // If no rule applied, keep the branch as is
      }
    }
    tableau = nextTableau;
  }

  return tableau.map((branch) => branch.formulas);
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
    case 'biconditional':
      return `(${formulaToString(f.left)} ↔ ${formulaToString(f.right)})`;
    case 'predicate':
      return `${f.symbol}(${f.terms.map((t) => t.symbol).join(', ')})`;
    case 'universal':
      return `∀${f.variable} ${formulaToString(f.formula)}`;
    case 'existential':
      return `∃${f.variable} ${formulaToString(f.formula)}`;
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

  const P = predicate('P', [term('x')]);
  const Q = predicate('Q', [term('x')]);
  testArgument('∀x P(x) ⊢ P(a)', [forAll('x', P)], substitute(P, 'x', term('a')));
  testArgument(
    '∃x P(x), ∀x (P(x) → Q(x)) ⊢ ∃x Q(x)',
    [exists('x', P), forAll('x', implies(P, Q))],
    exists('x', Q)
  );
}

main();
