// ================ Formula Types ================

type Formula =
  | { type: 'proposition'; symbol: string }
  | { type: 'negation'; formula: Formula }
  | { type: 'conjunction'; left: Formula; right: Formula }
  | { type: 'disjunction'; left: Formula; right: Formula }
  | { type: 'implication'; left: Formula; right: Formula };

// ================ Formula Creation Helpers ================

const prop = (symbol: string): Formula => ({ type: 'proposition', symbol });
const not = (formula: Formula): Formula => ({ type: 'negation', formula });
const and = (left: Formula, right: Formula): Formula => ({ type: 'conjunction', left, right });
const or = (left: Formula, right: Formula): Formula => ({ type: 'disjunction', left, right });
const implies = (left: Formula, right: Formula): Formula => ({ type: 'implication', left, right });

// ================ Tableau Logic ================

const isAtomic = (formula: Formula): boolean =>
  formula.type === 'proposition' ||
  (formula.type === 'negation' && formula.formula.type === 'proposition');

const isClosed = (branch: Formula[]): boolean =>
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

const isAlpha = (formula: Formula): boolean =>
  formula.type === 'conjunction' ||
  (formula.type === 'negation' &&
    ['disjunction', 'implication', 'negation'].includes(formula.formula.type));

const isBeta = (formula: Formula): boolean =>
  ['disjunction', 'implication'].includes(formula.type) ||
  (formula.type === 'negation' && formula.formula.type === 'conjunction');

const getAlphaComponents = (formula: Formula): Formula[] => {
  switch (formula.type) {
    case 'conjunction':
      return [formula.left, formula.right];
    case 'negation': {
      const inner = formula.formula;
      if (inner.type === 'disjunction') {
        return [not(inner.left), not(inner.right)];
      } else if (inner.type === 'implication') {
        return [inner.left, not(inner.right)];
      } else if (inner.type === 'negation') {
        return [inner.formula];
      }
      return [];
    }
    default:
      return [];
  }
};

const getBetaComponents = (formula: Formula): [Formula, Formula] => {
  switch (formula.type) {
    case 'disjunction':
      return [formula.left, formula.right];
    case 'implication':
      return [not(formula.left), formula.right];
    case 'negation': {
      if (formula.formula.type === 'conjunction') {
        return [formula.formula.left, formula.formula.right].map(not) as [Formula, Formula];
      }
      throw new Error('Invalid beta formula');
    }
    default:
      throw new Error('Invalid beta formula');
  }
};

// ================ Tableau Building ================

const buildTableau = (formulas: Formula[]): Formula[][] => {
  let tableau: Formula[][] = [formulas];
  let expanded = true;

  while (expanded) {
    expanded = false;
    tableau = tableau.flatMap((branch) => {
      if (isClosed(branch)) return [branch];

      let foundExpandable = false;
      for (let i = 0; i < branch.length && !foundExpandable; i++) {
        const formula = branch[i];
        // Ensure formula is defined (TypeScript safety)
        if (!formula) continue;

        if (isAtomic(formula)) continue;

        if (isAlpha(formula)) {
          // Handle alpha formulas
          expanded = true;
          foundExpandable = true;
          const rest = branch.filter((_, idx) => idx !== i);
          const components = getAlphaComponents(formula);
          return [rest.concat(components)];
        } else if (isBeta(formula)) {
          // Handle beta formulas
          expanded = true;
          foundExpandable = true;
          const rest = branch.filter((_, idx) => idx !== i);
          const [left, right] = getBetaComponents(formula);
          return [rest.concat(left), rest.concat(right)];
        }
      }
      return [branch];
    });
  }

  return tableau;
};

// ================ Satisfiability and Validity Check ================

const checkSatisfiability = (
  formulas: Formula[]
): { satisfiable: boolean; model?: Record<string, boolean> } => {
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
};

const checkValidity = (premises: Formula[], conclusion: Formula): boolean => {
  const { satisfiable } = checkSatisfiability([...premises, not(conclusion)]);
  return !satisfiable;
};

// ================ Utility Functions ================

const formulaToString = (formula: Formula): string => {
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
};

const printTableau = (tableau: Formula[][]): void => {
  tableau.forEach((branch, i) => {
    console.log(`Branch ${i + 1}:`);
    branch.forEach((f) => console.log(`- ${formulaToString(f)}`));
    console.log(`Closed: ${isClosed(branch)}\n`);
  });
};

const testArgument = (name: string, premises: Formula[], conclusion: Formula): void => {
  console.log(`Testing validity of ${name}`);
  premises.forEach((f) => console.log(`- ${formulaToString(f)}`));
  console.log(`Conclusion: ${formulaToString(conclusion)}`);
  const isValid = checkValidity(premises, conclusion);
  console.log(`The argument is ${isValid ? 'valid' : 'invalid'}.`);
};

// ================ Example Usage ================

const testDisjunctiveSyllogism = () => {
  const p = prop('p');
  const q = prop('q');
  testArgument('p∨q, ¬p |= q', [or(p, q), not(p)], q);
};

const testModusPonens = () => {
  const p = prop('p');
  const q = prop('q');
  testArgument('p→q, p |= q', [implies(p, q), p], q);
};

const testModusTollens = () => {
  const p = prop('p');
  const q = prop('q');
  testArgument('p→q, ¬q |= ¬p', [implies(p, q), not(q)], not(p));
};

const testHypotheticalSyllogism = () => {
  const p = prop('p');
  const q = prop('q');
  const r = prop('r');
  testArgument('p→q, q→r |= p→r', [implies(p, q), implies(q, r)], implies(p, r));
};

const main = () => {
  console.log('Tableau Method for Propositional Logic\n=====================================');
  testDisjunctiveSyllogism();
  testModusPonens();
  testModusTollens();
  testHypotheticalSyllogism();
};

main();
