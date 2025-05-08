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
  | Predicate
  | Universal
  | Existential;

// Special formula types for type guards
type NegatedDisjunction = Negation & { formula: Disjunction };
type NegatedImplication = Negation & { formula: Implication };
type DoubleNegation = Negation & { formula: Negation };
type NegatedConjunction = Negation & { formula: Conjunction };

// String prototype extension for hashing
declare global {
  interface String {
    hashCode(): number;
  }
}

// Add hashCode method to String prototype
String.prototype.hashCode = function (): number {
  let hash = 0;
  for (let i = 0; i < this.length; i++) {
    const char = this.charCodeAt(i);
    hash = (hash << 5) - hash + char;
    hash = hash & hash; // Convert to 32bit integer
  }
  return hash;
};

// ================ Type Guards ===================

export const isProposition = (f: Formula): f is Proposition => f.type === 'proposition';
export const isNegation = (f: Formula): f is Negation => f.type === 'negation';
export const isConjunction = (f: Formula): f is Conjunction => f.type === 'conjunction';
export const isDisjunction = (f: Formula): f is Disjunction => f.type === 'disjunction';
export const isImplication = (f: Formula): f is Implication => f.type === 'implication';
export const isPredicate = (f: Formula): f is Predicate => f.type === 'predicate';
export const isUniversal = (f: Formula): f is Universal => f.type === 'universal';
export const isExistential = (f: Formula): f is Existential => f.type === 'existential';
export const isTerm = (t: any): t is Term => t && t.type === 'term';

export const isNegatedDisjunction = (f: Formula): f is NegatedDisjunction =>
  isNegation(f) && isDisjunction(f.formula);

export const isNegatedImplication = (f: Formula): f is NegatedImplication =>
  isNegation(f) && isImplication(f.formula);

export const isDoubleNegation = (f: Formula): f is DoubleNegation =>
  isNegation(f) && isNegation(f.formula);

export const isNegatedConjunction = (f: Formula): f is NegatedConjunction =>
  isNegation(f) && isConjunction(f.formula);

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

// Extract all terms from formulas in a branch
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
    } else if (isConjunction(formula) || isDisjunction(formula) || isImplication(formula)) {
      collect(formula.left);
      collect(formula.right);
    } else if (isUniversal(formula) || isExistential(formula)) {
      collect(formula.formula);
    }
  }

  formulas.forEach(collect);
  return result;
}

// Generate a fresh constant not in the existing terms
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

// Substitute a term for a variable in a formula
export function substitute(formula: Formula, variable: string, termToSubstitute: Term): Formula {
  if (isPredicate(formula)) {
    // Check if any terms are variables that need substitution
    const newTerms = formula.terms.map((t) => (t.symbol === variable ? termToSubstitute : t));
    return predicate(formula.symbol, newTerms);
  } else if (isProposition(formula)) {
    return formula; // No substitution needed in propositions
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
  } else if (isUniversal(formula)) {
    // Don't substitute if the variable is bound by this quantifier
    if (formula.variable === variable) return formula;
    return forAll(formula.variable, substitute(formula.formula, variable, termToSubstitute));
  } else if (isExistential(formula)) {
    // Don't substitute if the variable is bound by this quantifier
    if (formula.variable === variable) return formula;
    return exists(formula.variable, substitute(formula.formula, variable, termToSubstitute));
  }
  return formula;
}

// Compare formulas for equality
export function formulaEquals(f1: Formula, f2: Formula): boolean {
  return formulaToString(f1) === formulaToString(f2);
}

// ================ Rule Definitions ===================

type RuleType = 'alpha' | 'beta' | 'gamma' | 'delta';

export interface TableauContext {
  terms?: Term[];
  branch?: Formula[];
}

export interface FormulaRule {
  ruleType: RuleType;
  description: string;
  matches(f: Formula): boolean;
  getComponents(f: Formula, context?: TableauContext): Formula[];
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
  {
    ruleType: 'gamma',
    description: 'Universal quantifier: ∀x P(x)',
    matches: isUniversal,
    getComponents: (f: Universal, context) => {
      // Gamma rule needs existing terms to instantiate
      if (!context || !context.terms || context.terms.length === 0) {
        return [];
      }

      // Create an instance for each term
      return context.terms.map((t) => substitute(f.formula, f.variable, t));
    },
  },
  {
    ruleType: 'delta',
    description: 'Existential quantifier: ∃x P(x)',
    matches: isExistential,
    getComponents: (f: Existential, context) => {
      // Delta rule creates a fresh constant
      if (!context || !context.terms) {
        // If no context is provided, create a default fresh constant
        const freshTerm = term('c1');
        return [substitute(f.formula, f.variable, freshTerm)];
      }

      // Create a fresh constant not appearing in context
      const freshTerm = freshConstant(context.terms);
      return [substitute(f.formula, f.variable, freshTerm)];
    },
  },
];

// ================ Utilities ===================

export const isAtomic = (f: Formula): boolean =>
  isProposition(f) ||
  isPredicate(f) ||
  (isNegation(f) && (isProposition(f.formula) || isPredicate(f.formula)));

export const isClosed = (branch: Formula[]): boolean => {
  // Check for propositional contradictions
  for (const f1 of branch) {
    if (isProposition(f1)) {
      for (const f2 of branch) {
        if (isNegation(f2) && isProposition(f2.formula) && f1.symbol === f2.formula.symbol) {
          return true;
        }
      }
    }
  }

  // Check for predicate contradictions
  for (const f1 of branch) {
    if (isPredicate(f1)) {
      for (const f2 of branch) {
        if (
          isNegation(f2) &&
          isPredicate(f2.formula) &&
          f1.symbol === f2.formula.symbol &&
          f1.terms.length === f2.formula.terms.length &&
          f1.terms.every((t, i) => t.symbol === f2.formula.terms[i].symbol)
        ) {
          return true;
        }
      }
    }
  }

  return false;
};

function findRule(f: Formula): FormulaRule | undefined {
  return formulaRules.find((r) => r.matches(f));
}

export const isAlpha = (f: Formula): boolean => findRule(f)?.ruleType === 'alpha';
export const isBeta = (f: Formula): boolean => findRule(f)?.ruleType === 'beta';
export const isGamma = (f: Formula): boolean => findRule(f)?.ruleType === 'gamma';
export const isDelta = (f: Formula): boolean => findRule(f)?.ruleType === 'delta';

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

export function getGammaComponents(f: Formula, terms: Term[]): Formula[] {
  const rule = findRule(f);
  if (!rule || rule.ruleType !== 'gamma')
    throw new Error(`Not a gamma formula: ${formulaToString(f)}`);
  return rule.getComponents(f, { terms });
}

export function getDeltaComponents(f: Formula, terms: Term[]): Formula[] {
  const rule = findRule(f);
  if (!rule || rule.ruleType !== 'delta')
    throw new Error(`Not a delta formula: ${formulaToString(f)}`);
  return rule.getComponents(f, { terms });
}

export function getComponents(f: Formula, context?: TableauContext): Formula[] {
  const rule = findRule(f);
  return rule ? rule.getComponents(f, context) : [];
}

// ================ Tableau Construction ===================

export function buildTableau(premises: Formula[]): Formula[][] {
  // Initialize with a single branch containing all premises
  let branches: Formula[][] = [premises];
  // Store fully expanded branches here
  const completedBranches: Formula[][] = [];

  // Track applied gamma instantiations per branch to prevent loops
  // We'll use a Map where keys are branch signatures and values are Sets of formula signatures
  const branchGammaTracker = new Map<string, Set<string>>();

  // Safety counter for iterations
  let iterationCount = 0;
  const MAX_ITERATIONS = 100;

  // Continue processing until all branches are completed or max iterations reached
  while (branches.length > 0) {
    // Safety check
    iterationCount++;
    if (iterationCount > MAX_ITERATIONS) {
      console.log('Maximum iterations reached - terminating tableau construction');
      // Move all remaining branches to completed
      completedBranches.push(...branches);
      break;
    }

    // Take the first branch to process
    const currentBranch = branches.shift()!;

    // If branch is closed, add to completed and continue
    if (isClosed(currentBranch)) {
      completedBranches.push(currentBranch);
      continue;
    }

    // Create a branch signature for tracking gamma rule applications
    const branchSignature = currentBranch.map(formulaToString).join('|');
    if (!branchGammaTracker.has(branchSignature)) {
      branchGammaTracker.set(branchSignature, new Set<string>());
    }

    // Try to apply a rule
    let ruleApplied = false;

    // 1. Try alpha rules first (non-branching)
    for (let i = 0; i < currentBranch.length; i++) {
      const formula = currentBranch[i];

      if (isAtomic(formula)) continue;
      if (!isAlpha(formula)) continue;

      const alphaComponents = getAlphaComponents(formula);

      // Check if all components are already in the branch
      const allExist = alphaComponents.every((comp) =>
        currentBranch.some((f) => formulaEquals(f, comp))
      );

      if (!allExist) {
        // Create a new branch with alpha components added
        const newBranch = [...currentBranch];
        for (const comp of alphaComponents) {
          if (!newBranch.some((f) => formulaEquals(f, comp))) {
            newBranch.push(comp);
          }
        }

        branches.unshift(newBranch);
        ruleApplied = true;
        break;
      }
    }

    // 2. If no alpha rule applied, try beta rules (branching)
    if (!ruleApplied) {
      for (let i = 0; i < currentBranch.length; i++) {
        const formula = currentBranch[i];

        if (isAtomic(formula)) continue;
        if (!isBeta(formula)) continue;

        const betaComponents = getBetaComponents(formula);

        // Check if either component is already in the branch
        const leftExists = currentBranch.some((f) => formulaEquals(f, betaComponents[0]));
        const rightExists = currentBranch.some((f) => formulaEquals(f, betaComponents[1]));

        if (!leftExists || !rightExists) {
          // Create two new branches
          if (!leftExists) {
            const leftBranch = [...currentBranch, betaComponents[0]];
            branches.unshift(leftBranch);
          }

          if (!rightExists) {
            const rightBranch = [...currentBranch, betaComponents[1]];
            branches.unshift(rightBranch);
          }

          ruleApplied = true;
          break;
        }
      }
    }

    // 3. If no alpha/beta rule applied, try delta rules (existential)
    if (!ruleApplied) {
      for (let i = 0; i < currentBranch.length; i++) {
        const formula = currentBranch[i];

        if (!isExistential(formula)) continue;

        // Get all existing terms in the branch
        const terms = extractTerms(currentBranch);

        // Generate a fresh constant and create instantiation
        const freshTerm = freshConstant(terms);
        const instantiation = substitute(formula.formula, formula.variable, freshTerm);

        // Create new branch with the existential formula consumed
        const newBranch = [...currentBranch];
        newBranch.splice(i, 1); // Remove existential formula
        newBranch.push(instantiation); // Add instantiation

        branches.unshift(newBranch);
        ruleApplied = true;
        break;
      }
    }

    // 4. If no alpha/beta/delta rule applied, try gamma rules (universal)
    if (!ruleApplied) {
      const universalFormulas = currentBranch.filter(isUniversal);
      if (universalFormulas.length > 0) {
        const terms = extractTerms(currentBranch);

        // Try each universal formula
        for (const universalFormula of universalFormulas) {
          const formula = universalFormula as Universal;
          const formulaStr = formulaToString(formula);

          // Try each term
          for (const t of terms) {
            // Create a unique identifier for this instantiation
            const instantiationKey = `${formulaStr}_${t.symbol}`;

            // Check if we've already applied this instantiation on this branch
            const gammaSet = branchGammaTracker.get(branchSignature)!;
            if (!gammaSet.has(instantiationKey)) {
              // Create the instantiated formula
              const instantiated = substitute(formula.formula, formula.variable, t);

              // Check if it's already in the branch
              if (!currentBranch.some((f) => formulaEquals(f, instantiated))) {
                // Add instantiation to the branch
                const newBranch = [...currentBranch, instantiated];
                branches.unshift(newBranch);

                // Mark this instantiation as applied for this branch
                gammaSet.add(instantiationKey);
                ruleApplied = true;
                break;
              } else {
                // Mark it as applied even though we didn't add it (since it's already there)
                gammaSet.add(instantiationKey);
              }
            }
          }

          if (ruleApplied) break;
        }
      }
    }

    // If no rules could be applied, this branch is fully expanded
    if (!ruleApplied) {
      completedBranches.push(currentBranch);
    }
  }

  return completedBranches;
}

// ================ Satisfiability & Validity ===================

export function checkSatisfiability(formulas: Formula[]): {
  satisfiable: boolean;
  model?: Record<string, boolean>;
} {
  const tableau = buildTableau(formulas);

  for (const branch of tableau) {
    if (!isClosed(branch)) {
      // Build a model from the open branch
      const model: Record<string, boolean> = {};

      // Extract truth values for propositions
      for (const f of branch) {
        if (isProposition(f)) {
          model[f.symbol] = true;
        } else if (isNegation(f) && isProposition(f.formula)) {
          model[f.formula.symbol] = false;
        }
      }

      // For first-order logic, we would need a more complex model structure
      // This is a simplification for propositional logic
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
    case 'predicate':
      return `${f.symbol}(${f.terms.map((t) => t.symbol).join(', ')})`;
    case 'universal':
      return `∀${f.variable}(${formulaToString(f.formula)})`;
    case 'existential':
      return `∃${f.variable}(${formulaToString(f.formula)})`;
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
  // Test propositional logic
  const p = prop('p');
  const q = prop('q');
  const r = prop('r');

  testArgument('p∨q, ¬p ⊢ q', [or(p, q), not(p)], q);
  testArgument('p→q, p ⊢ q', [implies(p, q), p], q);
  testArgument('p→q, ¬q ⊢ ¬p', [implies(p, q), not(q)], not(p));
  testArgument('p→q, q→r ⊢ p→r', [implies(p, q), implies(q, r)], implies(p, r));
  testArgument('⊢ (p∨(q∧r))→((p∨q)∧(p∨r))', [], implies(or(p, and(q, r)), and(or(p, q), or(p, r))));

  // Test first-order logic with gamma rule
  const x = 'x';
  const a = term('a');
  const b = term('b');

  const Px = predicate('P', [term(x)]);
  const Qx = predicate('Q', [term(x)]);
  const Pa = predicate('P', [a]);
  const Qa = predicate('Q', [a]);

  console.log('\nTesting Gamma Rule (Universal Quantifier):');
  testArgument('∀x(P(x) → Q(x)), P(a) ⊢ Q(a)', [forAll(x, implies(Px, Qx)), Pa], Qa);

  // Test first-order logic with delta rule
  console.log('\nTesting Delta Rule (Existential Quantifier):');
  testArgument('∃x P(x) ⊢ ¬∀x ¬P(x)', [exists(x, Px)], not(forAll(x, not(Px))));

  // Test combined gamma and delta rules
  console.log('\nTesting Combined Gamma and Delta Rules:');
  testArgument(
    '∀x(P(x) → Q(x)), ∃x P(x) ⊢ ∃x Q(x)',
    [forAll(x, implies(Px, Qx)), exists(x, Px)],
    exists(x, Qx)
  );
}

main();
