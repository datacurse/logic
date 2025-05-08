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

export interface Biconditional {
  type: 'biconditional';
  left: Formula;
  right: Formula;
}

// First-order logic types
export interface Term {
  type: 'term';
  symbol: string;
}

export interface Predicate {
  type: 'predicate';
  symbol: string;
  terms: Term[];
}

export interface Universal {
  type: 'universal';
  variable: string;
  formula: Formula;
}

export interface Existential {
  type: 'existential';
  variable: string;
  formula: Formula;
}

export type Formula =
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

// String prototype extension for hashing
declare global {
  interface String {
    hashCode(): number;
  }
}

// ================ Type Guards ===================

export const isProposition = (f: Formula): f is Proposition => f.type === 'proposition';
export const isNegation = (f: Formula): f is Negation => f.type === 'negation';
export const isConjunction = (f: Formula): f is Conjunction => f.type === 'conjunction';
export const isDisjunction = (f: Formula): f is Disjunction => f.type === 'disjunction';
export const isImplication = (f: Formula): f is Implication => f.type === 'implication';
export const isBiconditional = (f: Formula): f is Biconditional => f.type === 'biconditional';
export const isPredicate = (f: Formula): f is Predicate => f.type === 'predicate';
export const isUniversal = (f: Formula): f is Universal => f.type === 'universal';
export const isExistential = (f: Formula): f is Existential => f.type === 'existential';
export const isTerm = (t: any): t is Term => t && typeof t === 'object' && t.type === 'term';

export const isNegatedDisjunction = (f: Formula): f is NegatedDisjunction =>
  isNegation(f) && isDisjunction(f.formula);

export const isNegatedImplication = (f: Formula): f is NegatedImplication =>
  isNegation(f) && isImplication(f.formula);

export const isDoubleNegation = (f: Formula): f is DoubleNegation =>
  isNegation(f) && isNegation(f.formula);

export const isNegatedConjunction = (f: Formula): f is NegatedConjunction =>
  isNegation(f) && isConjunction(f.formula);

export const isNegatedUniversal = (f: Formula): f is NegatedUniversal =>
  isNegation(f) && isUniversal(f.formula);

export const isNegatedExistential = (f: Formula): f is NegatedExistential =>
  isNegation(f) && isExistential(f.formula);

export const isNegatedBiconditional = (f: Formula): f is NegatedBiconditional =>
  isNegation(f) && isBiconditional(f.formula);

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
export const iff = (left: Formula, right: Formula): Biconditional => ({
  type: 'biconditional',
  left,
  right,
});

// First-order logic helpers
export const term = (symbol: string): Term => ({ type: 'term', symbol });
export const predicate = (symbol: string, terms: Term[]): Predicate => ({
  type: 'predicate',
  symbol,
  terms,
});
export const forAll = (variable: string, formula: Formula): Universal => ({
  type: 'universal',
  variable,
  formula,
});
export const exists = (variable: string, formula: Formula): Existential => ({
  type: 'existential',
  variable,
  formula,
});

// ================ Term Utilities ===================

export function extractTerms(formulas: Formula[]): Term[] {
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

export function freshConstant(existingTerms: Term[]): Term {
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

export function substitute(formula: Formula, variable: string, termToSubstitute: Term): Formula {
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

export function formulaEquals(f1: Formula, f2: Formula): boolean {
  // This is a structural equality check based on the string representation
  // A more robust check would traverse the structure recursively.
  // For tableau purposes, string equality is usually sufficient if string conversion is canonical.
  return formulaToString(f1) === formulaToString(f2);
}

// ================ Rule Definitions ===================

type RuleType = 'alpha' | 'beta' | 'gamma' | 'delta';

export interface TableauContext {
  terms: Term[];
  // branch?: Formula[]; // Not currently used by rule components, but could be for optimization
}

export interface FormulaRule {
  ruleType: RuleType;
  description: string;
  matches(f: Formula): boolean;
  // getComponents should return Formula[] or [Formula, Formula] depending on the rule type
  // The actual implementation might need context (like terms)
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
      and(not(f.formula.left), f.formula.right), // Fixed syntax error: added closing parenthesis
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

export const isAtomic = (f: Formula): boolean =>
  isProposition(f) ||
  isPredicate(f) ||
  (isNegation(f) && (isProposition(f.formula) || isPredicate(f.formula)));

export function isClosed(branch: Formula[]): boolean {
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

export const isAlpha = (f: Formula): boolean => findRule(f)?.ruleType === 'alpha';
export const isBeta = (f: Formula): boolean => findRule(f)?.ruleType === 'beta';
export const isGamma = (f: Formula): boolean => findRule(f)?.ruleType === 'gamma';
export const isDelta = (f: Formula): boolean => findRule(f)?.ruleType === 'delta';

// Helper to get components with proper type assertion for Alpha rules
function getAlphaComponentsInternal(f: Formula): Formula[] {
  const rule = formulaRules.find((r) => r.matches(f) && r.ruleType === 'alpha');
  if (!rule) {
    // This case should ideally not happen if isAlpha was checked before calling
    throw new Error(`No alpha rule found for formula: ${formulaToString(f)}`);
  }
  // Type assertion based on the filter above
  return rule.getComponents(f) as Formula[];
}

// Helper to get components with proper type assertion for Beta rules
function getBetaComponentsInternal(f: Formula): [Formula, Formula] {
  const rule = formulaRules.find((r) => r.matches(f) && r.ruleType === 'beta');
  if (!rule) {
    // This case should ideally not happen if isBeta was checked before calling
    throw new Error(`No beta rule found for formula: ${formulaToString(f)}`);
  }
  const components = rule.getComponents(f);
  if (!Array.isArray(components) || components.length !== 2) {
    throw new Error(`Beta rule did not return 2 components for ${formulaToString(f)}`);
  }
  // Type assertion based on the rule definition and the check above
  return components as [Formula, Formula];
}

// Helper to get components with proper type assertion for Gamma rules
function getGammaComponentsInternal(f: Formula, terms: Term[]): Formula[] {
  const rule = formulaRules.find((r) => r.matches(f) && r.ruleType === 'gamma');
  if (!rule) {
    // This case should ideally not happen if isGamma was checked before calling
    throw new Error(`No gamma rule found for formula: ${formulaToString(f)}`);
  }
  // Pass context with terms
  const components = rule.getComponents(f, { terms });
  if (!Array.isArray(components)) {
    throw new Error(`Gamma rule did not return an array of components for ${formulaToString(f)}`);
  }
  return components as Formula[];
}

// Helper to get components with proper type assertion for Delta rules
function getDeltaComponentsInternal(f: Formula, terms: Term[]): Formula[] {
  const rule = formulaRules.find((r) => r.matches(f) && r.ruleType === 'delta');
  if (!rule) {
    // This case should ideally not happen if isDelta was checked before calling
    throw new Error(`No delta rule found for formula: ${formulaToString(f)}`);
  }
  // Pass context with terms
  const components = rule.getComponents(f, { terms });
  if (!Array.isArray(components)) {
    throw new Error(`Delta rule did not return an array of components for ${formulaToString(f)}`);
  }
  return components as Formula[];
}

// These public functions can be used, but the internal ones are used by buildTableau for type safety
export function getAlphaComponents(f: Formula): Formula[] {
  return getAlphaComponentsInternal(f);
}

export function getBetaComponents(f: Formula): [Formula, Formula] {
  return getBetaComponentsInternal(f);
}

export function getGammaComponents(f: Formula, terms: Term[]): Formula[] {
  return getGammaComponentsInternal(f, terms);
}

export function getDeltaComponents(f: Formula, terms: Term[]): Formula[] {
  return getDeltaComponentsInternal(f, terms);
}

// Generic getComponents - less specific type
export function getComponents(
  f: Formula,
  context?: TableauContext
): Formula[] | [Formula, Formula] | undefined {
  const rule = findRule(f);
  return rule ? rule.getComponents(f, context) : undefined;
}

// ================ Tableau Construction ===================

interface BranchState {
  formulas: Formula[];
  gammaSet: Set<string>;
  deltaSet: Set<string>;
  betaProcessedSet: Set<string>;
}

export function buildTableau(premises: Formula[], maxIterations: number = 1000): Formula[][] {
  let branches: BranchState[] = [
    {
      formulas: [...premises],
      gammaSet: new Set(),
      deltaSet: new Set(),
      betaProcessedSet: new Set(),
    },
  ];

  const completedBranches: Formula[][] = [];
  let iterationCount = 0;

  while (branches.length > 0 && iterationCount <= maxIterations) {
    iterationCount++;
    const currentBranch = branches.shift()!; // Use '!' as we check branches.length > 0

    if (isClosed(currentBranch.formulas)) {
      completedBranches.push(currentBranch.formulas);
      continue;
    }

    let ruleApplied = false;

    // Prioritize Alpha and Delta rules (deterministic)

    // 1. Apply Alpha rules
    alpha: for (const formula of currentBranch.formulas) {
      if (!isAlpha(formula)) continue;

      const components = getAlphaComponentsInternal(formula);
      const actualFormulasToAdd: Formula[] = [];
      for (const comp of components) {
        if (!currentBranch.formulas.some((f_exist) => formulaEquals(f_exist, comp))) {
          actualFormulasToAdd.push(comp);
        }
      }

      if (actualFormulasToAdd.length > 0) {
        // Add components to the current branch and put it back at the front
        branches.unshift({
          formulas: [...currentBranch.formulas, ...actualFormulasToAdd],
          gammaSet: new Set(currentBranch.gammaSet),
          deltaSet: new Set(currentBranch.deltaSet),
          betaProcessedSet: new Set(currentBranch.betaProcessedSet),
        });
        ruleApplied = true;
        break alpha; // Restart iteration on the modified branch
      }
    }
    if (ruleApplied) continue;

    // 2. Apply Delta rules
    delta: for (const formula of currentBranch.formulas) {
      if (!isExistential(formula)) continue;

      const key = formulaToString(formula);
      if (!currentBranch.deltaSet.has(key)) {
        const terms = extractTerms(currentBranch.formulas);
        const fresh = freshConstant(terms);
        const instantiated = substitute(formula.formula, formula.variable, fresh);

        // Add the instantiated formula and mark the existential as processed for this branch
        branches.unshift({
          formulas: [...currentBranch.formulas, instantiated],
          gammaSet: new Set(currentBranch.gammaSet),
          deltaSet: new Set([...currentBranch.deltaSet, key]),
          betaProcessedSet: new Set(currentBranch.betaProcessedSet),
        });
        ruleApplied = true;
        break delta; // Restart iteration on the modified branch
      }
    }
    if (ruleApplied) continue;

    // Apply Gamma and Beta rules (non-deterministic or require existing terms)
    // We process these after deterministic rules

    // 3. Apply Gamma rules
    // Gamma rules can be applied multiple times with different terms
    gamma: for (const formula of currentBranch.formulas) {
      if (!isUniversal(formula)) continue;

      const currentTermsOnBranch = extractTerms(currentBranch.formulas);

      const termsToInstantiateWith =
        currentTermsOnBranch.length === 0
          ? [freshConstant([])] // If no terms, introduce one fresh constant
          : currentTermsOnBranch; // Otherwise, use all existing terms

      let instantiatedFormulas: Formula[] = [];
      let newGammaKeys: string[] = [];
      let appliedForGamma = false;

      for (const term_instance of termsToInstantiateWith) {
        const key = `${formulaToString(formula)}_${formulaToString(term_instance)}`; // Use term string for key
        if (!currentBranch.gammaSet.has(key)) {
          const instantiated = substitute(formula.formula, formula.variable, term_instance);
          if (!currentBranch.formulas.some((f_exist) => formulaEquals(f_exist, instantiated))) {
            instantiatedFormulas.push(instantiated);
            newGammaKeys.push(key);
            appliedForGamma = true;
            // Applying Gamma rule with one term is enough to make progress
            // If multiple terms exist, we could add them all in one go,
            // but applying one at a time simplifies the loop structure and ensures progress.
            // Let's add all applicable instantiations for existing terms at once.
            // If no terms existed, we add the single fresh constant instantiation.
          } else {
            // If the instantiated formula is already present, mark the gamma instance as processed
            newGammaKeys.push(key);
          }
        }
      }

      if (instantiatedFormulas.length > 0 || newGammaKeys.length > 0) {
        branches.unshift({
          formulas: [...currentBranch.formulas, ...instantiatedFormulas],
          gammaSet: new Set([...currentBranch.gammaSet, ...newGammaKeys]),
          deltaSet: new Set(currentBranch.deltaSet),
          betaProcessedSet: new Set(currentBranch.betaProcessedSet),
        });
        ruleApplied = true;
        break gamma; // Restart iteration on the modified branch
      }
    }
    if (ruleApplied) continue;

    // 4. Apply Beta rules
    // Beta rules split the branch, so they should be done last if possible
    beta: for (const formula of currentBranch.formulas) {
      if (!isBeta(formula)) continue;

      const formulaStr = formulaToString(formula);
      if (currentBranch.betaProcessedSet.has(formulaStr)) {
        continue; // Already applied beta rule to this formula on this branch
      }

      const [comp1, comp2] = getBetaComponentsInternal(formula);

      // Create two new branches
      const branch1Formulas = currentBranch.formulas.some((f_exist) =>
        formulaEquals(f_exist, comp1)
      )
        ? [...currentBranch.formulas]
        : [...currentBranch.formulas, comp1];

      const branch1State: BranchState = {
        formulas: branch1Formulas,
        gammaSet: new Set(currentBranch.gammaSet),
        deltaSet: new Set(currentBranch.deltaSet),
        betaProcessedSet: new Set([...currentBranch.betaProcessedSet, formulaStr]),
      };

      const branch2Formulas = currentBranch.formulas.some((f_exist) =>
        formulaEquals(f_exist, comp2)
      )
        ? [...currentBranch.formulas]
        : [...currentBranch.formulas, comp2];

      const branch2State: BranchState = {
        formulas: branch2Formulas,
        gammaSet: new Set(currentBranch.gammaSet),
        deltaSet: new Set(currentBranch.deltaSet),
        betaProcessedSet: new Set([...currentBranch.betaProcessedSet, formulaStr]),
      };

      branches.unshift(branch1State, branch2State); // Add new branches to the front
      ruleApplied = true;
      break beta; // Restart iteration with the new branches
    }
    if (ruleApplied) continue;

    // If no rule was applied and the branch is not closed, it's an open branch
    completedBranches.push(currentBranch.formulas);
  }

  if (iterationCount > maxIterations && branches.length > 0) {
    console.warn(
      `Maximum iterations (${maxIterations}) reached. Tableau might be incomplete (remaining open branches).`
    );
    // Add remaining branches to completedBranches, they are considered open if not closed
    branches.forEach((b) => completedBranches.push(b.formulas));
  } else if (iterationCount > maxIterations) {
    console.warn(`Maximum iterations (${maxIterations}) reached.`);
  }

  return completedBranches;
}

// ================ Satisfiability & Validity ===================

export function checkSatisfiability(formulas: Formula[]): {
  satisfiable: boolean;
  model?: Record<string, boolean>;
  openBranch?: Formula[];
} {
  const tableau = buildTableau(formulas);

  for (const branch of tableau) {
    if (!isClosed(branch)) {
      // Construct a model from an open branch (only for propositional symbols)
      const model: Record<string, boolean> = {};
      for (const f of branch) {
        if (isProposition(f)) {
          model[f.symbol] = true; // Assume proposition is true in this branch
        } else if (isNegation(f) && isProposition(f.formula)) {
          model[f.formula.symbol] = false; // Assume negated proposition is false
        }
        // Predicates and quantifiers are more complex to represent in a simple model object
      }
      return { satisfiable: true, model, openBranch: branch };
    }
  }

  return { satisfiable: false };
}

export function checkValidity(premises: Formula[], conclusion: Formula): boolean {
  // An argument premises ⊢ conclusion is valid if and only if premises ∧ ¬conclusion is unsatisfiable.
  // This is equivalent to checking if the tableau for premises ∪ {¬conclusion} closes.
  const formulasToRefute = [...premises, not(conclusion)];
  const tableau = buildTableau(formulasToRefute);
  return tableau.every((branch) => isClosed(branch));
}

// ================ Utilities & Testing ===================

export function formulaToString(f: Formula | Term): string {
  if (isTerm(f)) {
    return f.symbol;
  }

  switch (f.type) {
    case 'proposition':
      return f.symbol;
    case 'negation':
      const innerStr = formulaToString(f.formula);
      // Add parentheses for clarity if the inner formula is a binary connective or quantifier or another negation
      if (
        isConjunction(f.formula) ||
        isDisjunction(f.formula) ||
        isImplication(f.formula) ||
        isBiconditional(f.formula) ||
        isUniversal(f.formula) ||
        isExistential(f.formula) ||
        isNegation(f.formula)
      ) {
        return `¬(${innerStr})`;
      }
      return `¬${innerStr}`;
    case 'conjunction':
      return `(${formulaToString(f.left)} ∧ ${formulaToString(f.right)})`;
    case 'disjunction':
      return `(${formulaToString(f.left)} ∨ ${formulaToString(f.right)})`;
    case 'implication':
      return `(${formulaToString(f.left)} → ${formulaToString(f.right)})`;
    case 'biconditional':
      return `(${formulaToString(f.left)} ↔ ${formulaToString(f.right)})`;
    case 'predicate':
      return `${f.symbol}(${f.terms.map((t) => formulaToString(t)).join(', ')})`;
    case 'universal':
      return `∀${f.variable}(${formulaToString(f.formula)})`;
    case 'existential':
      return `∃${f.variable}(${formulaToString(f.formula)})`;
    default:
      // This should ideally not be reached if the Formula type covers all cases
      const exhaustiveCheck: never = f;
      throw new Error(`Unknown formula type: ${JSON.stringify(exhaustiveCheck)}`);
  }
}

export function printTableau(tableau: Formula[][]): void {
  tableau.forEach((branch, i) => {
    console.log(`Branch ${i + 1}:`);
    branch.forEach((f) => console.log(`  - ${formulaToString(f)}`));
    console.log(`Closed: ${isClosed(branch)}`);
    if (!isClosed(branch)) {
      console.log('This branch is open.');
    }
    console.log('---');
  });
}

export function testArgument(
  name: string,
  premises: Formula[],
  conclusion: Formula,
  expected: boolean
): void {
  console.log(`\nTesting Argument: "${name}"`);
  if (premises.length > 0) {
    console.log('Premises:');
    premises.forEach((f) => console.log(`  - ${formulaToString(f)}`));
  } else {
    console.log('Premises: None (Proving a theorem)');
  }
  console.log(`Conclusion: ${formulaToString(conclusion)}`);
  const isValid = checkValidity(premises, conclusion);
  console.log(`Calculated Valid? ${isValid}`);
  console.log(`Expected Valid? ${expected}`);
  if (isValid === expected) {
    console.log('Result: CORRECT');
  } else {
    console.error('Result: INCORRECT');
    // For debugging incorrect results, print the tableau if the test fails:
    console.log('Tableau for INCORRECT result (refuting the argument):');
    const tableau = buildTableau([...premises, not(conclusion)], 2000); // Allow more iterations for debug
    printTableau(tableau);
  }
}

export function main(): void {
  const p = prop('p');
  const q = prop('q');
  const r = prop('r');

  testArgument('Modus Ponens: p → q, p ⊢ q', [implies(p, q), p], q, true);
  testArgument('Disjunctive Syllogism: p ∨ q, ¬p ⊢ q', [or(p, q), not(p)], q, true);
  testArgument(
    'Distribution: ⊢ (p∨(q∧r))→((p∨q)∧(p∨r))',
    [],
    implies(or(p, and(q, r)), and(or(p, q), or(p, r))),
    true
  );
  testArgument('Invalid: p → q, q ⊢ p', [implies(p, q), q], p, false);

  // Biconditional Tests
  testArgument(
    'Biconditional Intro: p → q, q → p ⊢ p ↔ q',
    [implies(p, q), implies(q, p)],
    iff(p, q),
    true
  );
  testArgument('Biconditional Elim 1: p ↔ q, p ⊢ q', [iff(p, q), p], q, true);
  testArgument('Biconditional Elim 2: p ↔ q, q ⊢ p', [iff(p, q), q], p, true);
  testArgument(
    'Negated Biconditional: ⊢ ¬(p ↔ q) ↔ ((p ∧ ¬q) ∨ (¬p ∧ q))',
    [],
    iff(not(iff(p, q)), or(and(p, not(q)), and(not(p), q))),
    true
  );

  const x = 'x';
  const y = 'y';
  const z = 'z';
  const a = term('a');

  const P = (t: Term) => predicate('P', [t]);
  const Q = (t: Term) => predicate('Q', [t]);
  const F = (t: Term) => predicate('F', [t]);
  const G = (t: Term) => predicate('G', [t]);
  const F_rel = (t1: Term, t2: Term) => predicate('F_rel', [t1, t2]);

  console.log('\n--- First-Order Logic Tests ---');

  testArgument(
    'Gamma Example: ∀x(P(x) → Q(x)), P(a) ⊢ Q(a)',
    [forAll(x, implies(P(term(x)), Q(term(x)))), P(a)],
    Q(a),
    true
  );

  testArgument(
    'Delta Example: ∃x P(x) ⊢ ¬∀x ¬P(x)',
    [exists(x, P(term(x)))],
    not(forAll(x, not(P(term(x))))),
    true
  );

  testArgument(
    'Negated Quantifier Equivalence: ¬∀x P(x) ⊢ ∃x ¬P(x)',
    [not(forAll(x, P(term(x))))],
    exists(x, not(P(term(x)))),
    true
  );

  testArgument(
    'Negated Quantifier Equivalence: ¬∃x P(x) ⊢ ∀x ¬P(x)',
    [not(exists(x, P(term(x))))],
    forAll(x, not(P(term(x)))),
    true
  );

  testArgument(
    'Combined Gamma & Delta: ∀x(P(x) → Q(x)), ∃x P(x) ⊢ ∃x Q(x)',
    [forAll(x, implies(P(term(x)), Q(term(x)))), exists(x, P(term(x)))],
    exists(x, Q(term(x))),
    true
  );

  testArgument(
    "Drinker's Paradox (Theorem): ⊢ ∃y∀x(F(y) → F(x))",
    [],
    exists(y, forAll(x, implies(F(term(y)), F(term(x))))),
    true
  );

  const R_pred = (t1: Term, t2: Term) => predicate('R', [t1, t2]);
  testArgument(
    'Syllogism-like FOL: ∀x(P(x) → Q(x)), ∀x(Q(x) → R(x,a)) ⊢ ∀x(P(x) → R(x,a))',
    [
      forAll(x, implies(P(term(x)), Q(term(x)))),
      forAll(x, implies(Q(term(x)), R_pred(term(x), a))),
    ],
    forAll(x, implies(P(term(x)), R_pred(term(x), a))),
    true
  );

  testArgument(
    'Invalid FOL: ∀x P(x) ⊢ ∃x Q(x)',
    [forAll(x, P(term(x)))],
    exists(x, Q(term(x))),
    false
  );

  testArgument(
    'Gamma with no initial terms: ⊢ ∀x P(x) → P(c1)',
    [],
    implies(forAll(x, P(term(x))), P(term('c1'))),
    true
  );

  // New Test Case 1 (Complex FOL with Biconditional)
  const Fx = F(term(x));
  const Gy = G(term(y));
  const Gz = G(term(z));
  const premiseComplex = exists(
    y,
    exists(
      z,
      forAll(
        x,
        and(
          implies(Fx, Gy), // Fx → Gy
          implies(Gz, Fx) // Gz → Fx
        )
      )
    )
  );
  const conclusionComplex = forAll(x, exists(y, iff(Fx, Gy))); // ∀x∃y(Fx↔Gy)
  testArgument(
    'Complex FOL: [∃y∃z∀x((Fx→Gy)∧(Gz→Fx))] ⊢ ∀x∃y(Fx↔Gy)', // Corrected name
    [premiseComplex], // Premises
    conclusionComplex, // Conclusion
    true // This is a known validity (related to choice principles / DGM theorem)
  );

  // New Test Case 2 (Quantifier Shift - Invalid)
  const premiseQuantShift = forAll(y, exists(x, F_rel(term(x), term(y))));
  const conclusionQuantShift = exists(x, forAll(y, F_rel(term(x), term(y))));
  testArgument(
    'Invalid Quantifier Shift: [∀y∃x F(x,y)] ⊢ ∃x∀y F(x,y)', // Corrected name
    [premiseQuantShift], // Premises
    conclusionQuantShift, // Conclusion
    false // This is generally invalid
  );

  console.log('\n--- Satisfiability Tests ---');
  const satResult1 = checkSatisfiability([P(a), not(P(a))]);
  console.log(`{P(a), ¬P(a)} Satisfiable? ${satResult1.satisfiable} (Expected: false)`);

  const satResult2 = checkSatisfiability([exists(x, P(term(x)))]);
  console.log(`{∃x P(x)} Satisfiable? ${satResult2.satisfiable} (Expected: true)`);
  if (satResult2.satisfiable) {
    console.log('Open branch (model hint):');
    // Limiting the number of formulas printed for brevity
    const formulasToPrint = satResult2.openBranch?.slice(0, 10) ?? [];
    formulasToPrint.forEach((f_model) => console.log(`  - ${formulaToString(f_model)}`));
    if (satResult2.openBranch && satResult2.openBranch.length > 10) {
      console.log(`  ... ${satResult2.openBranch.length - 10} more formulas`);
    }
  }
}

main();
