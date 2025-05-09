// ================ Formula Types ===================

type Proposition = { type: 'proposition'; symbol: string };
type Negation = { type: 'negation'; formula: Formula };
type Conjunction = { type: 'conjunction'; left: Formula; right: Formula };
type Disjunction = { type: 'disjunction'; left: Formula; right: Formula };
type Implication = { type: 'implication'; left: Formula; right: Formula };
type Biconditional = { type: 'biconditional'; left: Formula; right: Formula };
type Term = { type: 'term'; symbol: string };
type Predicate = { type: 'predicate'; symbol: string; terms: Term[] };
type Universal = { type: 'universal'; variable: string; formula: Formula };
type Existential = { type: 'existential'; variable: string; formula: Formula };
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
  isInnerType: (f: Formula) => f is T,
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
const and = (left: Formula, right: Formula): Conjunction => ({ type: 'conjunction', left, right });
const or = (left: Formula, right: Formula): Disjunction => ({ type: 'disjunction', left, right });
const implies = (left: Formula, right: Formula): Implication => ({ type: 'implication', left, right });
const iff = (left: Formula, right: Formula): Biconditional => ({ type: 'biconditional', left, right });

// First-order logic helpers
const term = (symbol: string): Term => ({ type: 'term', symbol });
const predicate = (symbol: string, terms: Term[]): Predicate => ({ type: 'predicate', symbol, terms });
const forAll = (variable: string, formula: Formula): Universal => ({ type: 'universal', variable, formula });
const exists = (variable: string, formula: Formula): Existential => ({ type: 'existential', variable, formula });

// ================ Term Utilities ===================

// Collects all distinct term symbols in the given formulas, then returns them as Term nodes.
function extractTerms(formulas: Formula[]): Term[] {
  const symbols = new Set<string>();

  function visit(f: Formula): void {
    switch (f.type) {
      case 'predicate':
        f.terms.forEach((t) => symbols.add(t.symbol));
        break;
      case 'negation':
        visit(f.formula);
        break;
      case 'conjunction':
      case 'disjunction':
      case 'implication':
      case 'biconditional':
        visit(f.left);
        visit(f.right);
        break;
      case 'universal':
      case 'existential':
        visit(f.formula);
        break;
    }
  }

  formulas.forEach(visit);
  return Array.from(symbols).map((symbol) => term(symbol));
}

// Picks the next available constant name c1, c2, … by inspecting numeric suffixes of existing terms.
function freshConstant(existing: Term[]): Term {
  // collect all numeric suffixes from names matching /^c(\d+)$/
  const used: number[] = [];
  for (const t of existing) {
    const m = t.symbol.match(/^c(\d+)$/);
    if (m && m[1] !== undefined) {
      used.push(parseInt(m[1], 10));
    }
  }

  // pick one higher than the current max, or 1 if none
  const next = used.length > 0 ? Math.max(...used) + 1 : 1;
  return term(`c${next}`);
}

// ================ Substitution ===================

function substitute(formula: Formula, variable: string, termToSubstitute: Term): Formula {
  switch (formula.type) {
    case 'proposition':
      return formula;
    case 'predicate':
      return {
        ...formula,
        terms: formula.terms.map((t) => (t.symbol === variable ? termToSubstitute : t)),
      };
    case 'negation':
      return {
        ...formula,
        formula: substitute(formula.formula, variable, termToSubstitute),
      };
    case 'conjunction':
    case 'disjunction':
    case 'implication':
    case 'biconditional': {
      // for binary connectives, left/right both need substituting
      const { left, right } = formula;
      return {
        ...formula,
        left: substitute(left, variable, termToSubstitute),
        right: substitute(right, variable, termToSubstitute),
      };
    }
    case 'universal':
    case 'existential':
      // if the quantifier binds the same variable, leave it alone
      if (formula.variable === variable) return formula;
      return {
        ...formula,
        formula: substitute(formula.formula, variable, termToSubstitute),
      };

    default:
      return formula;
  }
}

function formulaEquals(f1: Formula, f2: Formula): boolean {
  if (f1.type !== f2.type) return false;

  switch (f1.type) {
    case 'proposition':
      return f1.symbol === (f2 as Proposition).symbol;

    case 'predicate': {
      const p2 = f2 as Predicate;
      if (f1.terms.length !== p2.terms.length) return false;
      return f1.terms.every((t, i) => {
        const t2 = p2.terms[i];
        // guard against undefined:
        return t2 !== undefined && t.symbol === t2.symbol;
      });
    }

    case 'negation':
      return formulaEquals(f1.formula, (f2 as Negation).formula);

    case 'conjunction':
    case 'disjunction':
    case 'implication':
    case 'biconditional': {
      const { left: l1, right: r1 } = f1 as Conjunction & Disjunction & Implication & Biconditional;
      const { left: l2, right: r2 } = f2 as Conjunction & Disjunction & Implication & Biconditional;
      return formulaEquals(l1, l2) && formulaEquals(r1, r2);
    }

    case 'universal':
    case 'existential': {
      const q1 = f1 as Universal | Existential;
      const q2 = f2 as Universal | Existential;
      return q1.variable === q2.variable && formulaEquals(q1.formula, q2.formula);
    }

    default:
      // All Formula cases are covered above:
      return false;
  }
}

// ================ Rule Definitions ===================

type RuleType = 'alpha' | 'beta' | 'gamma' | 'delta';
type TableauContext = { terms: Term[] };
type FormulaRule = {
  ruleType: RuleType;
  description: string;
  matches(f: Formula): boolean;
  getComponents(f: Formula, context?: TableauContext): Formula[] | [Formula, Formula];
};

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
  isProposition(f) || isPredicate(f) || (isNegation(f) && (isProposition(f.formula) || isPredicate(f.formula)));

function isClosed(branch: Formula[]): boolean {
  const atomics = branch.filter(isAtomic);

  for (const f of atomics) {
    if (isNegation(f)) {
      if (atomics.some((g) => !isNegation(g) && formulaEquals(g, f.formula))) {
        return true;
      }
    } else {
      if (atomics.some((g) => isNegation(g) && formulaEquals(g.formula, f))) {
        return true;
      }
    }
  }

  return false;
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
  context?: TableauContext,
): Formula[] | [Formula, Formula] {
  const rule = formulaRules.find((r) => r.matches(f) && r.ruleType === ruleType);
  if (!rule) {
    throw new Error(`No ${ruleType} rule found for formula: ${formulaToString(f)}`);
  }
  return rule.getComponents(f, context);
}

function getAlphaComponents(f: Formula): Formula[] {
  return getComponentsForRule(f, 'alpha') as Formula[];
}

function getBetaComponents(f: Formula): [Formula, Formula] {
  return getComponentsForRule(f, 'beta') as [Formula, Formula];
}

function getGammaComponents(f: Formula, terms: Term[]): Formula[] {
  return getComponentsForRule(f, 'gamma', { terms }) as Formula[];
}

function getDeltaComponents(f: Formula, terms: Term[]): Formula[] {
  return getComponentsForRule(f, 'delta', { terms }) as Formula[];
}

function getComponents(f: Formula, context?: TableauContext): Formula[] | [Formula, Formula] | undefined {
  const rule = findRule(f);
  return rule ? rule.getComponents(f, context) : undefined;
}

// ================ Tableau Construction ===================

interface Branch {
  formulas: Formula[];
  gammaApplied: { quantifier: Formula; instance: Formula }[];
  deltaApplied: Formula[];
  betaApplied: Formula[];
}

function buildTableau(premises: Formula[]): Formula[][] {
  let tableau: Branch[] = [
    {
      formulas: [...premises],
      gammaApplied: [],
      deltaApplied: [],
      betaApplied: [],
    },
  ];

  let expanded = true;
  while (expanded) {
    expanded = false;
    const next: Branch[] = [];

    for (const branch of tableau) {
      if (isClosed(branch.formulas)) {
        next.push(branch);
        continue;
      }

      let didExpand = false;

      // ALPHA
      for (const f of branch.formulas) {
        if (isAlpha(f)) {
          const comps = getAlphaComponents(f).filter((c) => !branch.formulas.some((g) => formulaEquals(g, c)));
          if (comps.length) {
            next.push({
              ...branch,
              formulas: [...branch.formulas, ...comps],
            });
            didExpand = expanded = true;
            break;
          }
        }
      }
      if (didExpand) continue;

      // DELTA (∃)
      for (const f of branch.formulas) {
        if (isDelta(f) && !branch.deltaApplied.some((g) => formulaEquals(g, f))) {
          const terms = extractTerms(branch.formulas);
          const [inst] = getDeltaComponents(f, terms);
          next.push({
            ...branch,
            formulas: [...branch.formulas, inst],
            deltaApplied: [...branch.deltaApplied, f],
          });
          didExpand = expanded = true;
          break;
        }
      }
      if (didExpand) continue;

      // GAMMA (∀)
      for (const f of branch.formulas) {
        if (isGamma(f)) {
          const terms = extractTerms(branch.formulas);
          for (const inst of getGammaComponents(f, terms)) {
            const already = branch.gammaApplied.some(
              (pair) => formulaEquals(pair.quantifier, f) && formulaEquals(pair.instance, inst),
            );
            if (!already && !branch.formulas.some((g) => formulaEquals(g, inst))) {
              next.push({
                ...branch,
                formulas: [...branch.formulas, inst],
                gammaApplied: [...branch.gammaApplied, { quantifier: f, instance: inst }],
              });
              didExpand = expanded = true;
              break;
            }
          }
          if (didExpand) break;
        }
      }
      if (didExpand) continue;

      // BETA (branch split)
      for (const f of branch.formulas) {
        if (isBeta(f) && !branch.betaApplied.some((g) => formulaEquals(g, f))) {
          const [l, r] = getBetaComponents(f);
          next.push({
            ...branch,
            formulas: [...branch.formulas, l],
            betaApplied: [...branch.betaApplied, f],
          });
          next.push({
            ...branch,
            formulas: [...branch.formulas, r],
            betaApplied: [...branch.betaApplied, f],
          });
          didExpand = expanded = true;
          break;
        }
      }
      if (!didExpand) next.push(branch);
    }

    tableau = next;
  }

  return tableau.map((b) => b.formulas);
}

// ================ Satisfiability & Validity ===================

function checkSatisfiability(formulas: Formula[]): { satisfiable: boolean; model?: Record<string, boolean> } {
  const tableau = buildTableau(formulas);
  for (const branch of tableau) {
    if (!isClosed(branch)) {
      const model: Record<string, boolean> = {};
      for (const f of branch) {
        if (isProposition(f)) model[f.symbol] = true;
        if (isNegation(f) && isProposition(f.formula)) model[f.formula.symbol] = false;
      }
      return { satisfiable: true, model };
    }
  }
  return { satisfiable: false };
}

function checkValidity(premises: Formula[], conclusion: Formula): boolean {
  return !checkSatisfiability([...premises, not(conclusion)]).satisfiable;
}

// ================ Utilities & Testing ===================

function formulaToString(f: Formula): string {
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

function printTableau(tableau: Formula[][]): void {
  tableau.forEach((branch, i) => {
    console.log(`Branch ${i + 1}:`);
    branch.forEach((f) => console.log(`- ${formulaToString(f)}`));
    console.log(`Closed: ${isClosed(branch)}`);
  });
}

function testArgument(name: string, premises: Formula[], conclusion: Formula): void {
  console.log(`\n=== ${name} ===`);
  premises.forEach((f) => console.log(`P: ${formulaToString(f)}`));
  console.log(`C: ${formulaToString(conclusion)}`);
  console.log(`Valid? ${checkValidity(premises, conclusion)}`);
}

function main(): void {
  const p = prop('p'),
    q = prop('q'),
    r = prop('r');

  testArgument('p∨q, ¬p ⊢ q', [or(p, q), not(p)], q);
  testArgument('p→q, p ⊢ q', [implies(p, q), p], q);
  testArgument('p→q, ¬q ⊢ ¬p', [implies(p, q), not(q)], not(p));
  testArgument('p→q, q→r ⊢ p→r', [implies(p, q), implies(q, r)], implies(p, r));

  const P = predicate('P', [term('x')]);
  const Q = predicate('Q', [term('x')]);
  testArgument('∀x P(x) ⊢ P(a)', [forAll('x', P)], substitute(P, 'x', term('a')));
  testArgument('∃x P(x), ∀x (P(x) → Q(x)) ⊢ ∃x Q(x)', [exists('x', P), forAll('x', implies(P, Q))], exists('x', Q));
}

main();
