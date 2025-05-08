type ExpressionType = 'variable' | 'individual constant' | 'function symbol' | 'predicate' | 'world variable';

interface Formula {
  // Base class for all formula types (AtomicFormula, NegatedFormula, etc.)
}

class AtomicFormula implements Formula {
  constructor(public predicate: string, public terms: string[]) {}
}

class BinaryFormula implements Formula {
  constructor(public operator: string, public sub1: Formula, public sub2: Formula) {}
}

class QuantifiedFormula implements Formula {
  constructor(public quantifier: string, public variable: string, public matrix: Formula, public isModal: boolean = false) {}
}

class ModalFormula implements Formula {
  constructor(public operator: string, public sub: Formula) {}
}

class NegatedFormula implements Formula {
  constructor(public sub: Formula) {}
}

class Parser {
  symbols: string[] = [];
  expressionType: { [key: string]: string } = {}; // symbol => string describing expression type
  arities: { [key: string]: number } = {}; // symbol => number
  isModal: boolean = false;
  isPropositional: boolean = true;
  hasEquality: boolean = false;
  R: string | undefined;
  w: string | undefined;
  input: string | undefined;

  copy(): Parser {
    const nparser = new Parser();
    nparser.symbols = [...this.symbols];
    for (let i = 0; i < this.symbols.length; i++) {
      const sym = this.symbols[i];
      nparser.expressionType[sym] = this.expressionType[sym];
      nparser.arities[sym] = this.arities[sym];
    }
    nparser.isModal = this.isModal;
    nparser.isPropositional = this.isPropositional;
    nparser.hasEquality = this.hasEquality;
    nparser.R = this.R;
    nparser.w = this.w;
    return nparser;
  }

  registerExpression(ex: string, exType: string, arity: number): void {
    console.log('registering ' + exType + ' ' + ex);
    if (!this.expressionType[ex]) this.symbols.push(ex);
    else if (this.expressionType[ex] !== exType) {
      throw new Error(`Don't use '${ex}' as both ${this.expressionType[ex]} and ${exType}.`);
    }
    this.expressionType[ex] = exType;
    this.arities[ex] = arity;
  }

  getSymbols(expressionType: string): string[] {
    const res: string[] = [];
    for (const s of this.symbols) {
      if (this.expressionType[s].includes(expressionType)) res.push(s);
    }
    return res;
  }

  getNewSymbol(candidates: string, expressionType: string, arity: number): string {
    const candidateArr = candidates.split('');
    for (let i = 0; i < candidateArr.length; i++) {
      const sym = candidateArr[i];
      if (!this.expressionType[sym]) {
        this.registerExpression(sym, expressionType, arity);
        return sym;
      }
      candidates += candidates[0] + (i + 2);
    }
    return '';
  }

  getNewConstant(): string {
    return this.getNewSymbol('abcdefghijklmno', 'individual constant', 0);
  }

  getNewVariable(): string {
    return this.getNewSymbol('xyzwvutsr', 'variable', 0);
  }

  getNewFunctionSymbol(arity: number, isWorldFunction: boolean): string {
    const stype = `${arity}-ary function symbol` + (isWorldFunction ? ' for worlds' : '');
    return this.getNewSymbol('fghijklmn', stype, arity);
  }

  getNewWorldVariable(): string {
    return this.getNewSymbol('wvutsr', 'world variable', 0);
  }

  getNewWorldName(): string {
    return this.getNewSymbol('vutsr', 'world constant', 0);
  }

  getVariables(formula: any): string[] {
    const vars = new Set<string>();

    const extractVariables = (formula: any): void => {
      if (formula.sub) {
        extractVariables(formula.sub);
      } else if (formula.matrix) {
        extractVariables(formula.matrix);
      } else if (formula.sub1) {
        extractVariables(formula.sub1);
        extractVariables(formula.sub2);
      } else {
        const terms = formula.isArray ? formula : formula.terms;
        for (const term of terms) {
          if (term.isArray) {
            extractVariables(term);
          } else if (this.expressionType[term]?.includes('variable')) {
            vars.add(term);
          }
        }
      }
    };

    extractVariables(formula);
    return Array.from(vars);
  }

  isTseitinLiteral(formula: any): boolean {
    return this.expressionType[formula.predicate || formula.sub.predicate] === 'tseitin predicate';
  }

  initModality(): void {
    for (const sym of this.symbols) {
      if (this.expressionType[sym].includes('predicate')) {
        this.arities[sym] += 1;
      }
    }
    this.R = this.getNewSymbol('Rrℜ', '2-ary predicate', 2);
    this.w = this.getNewSymbol('wvur', 'world constant', 0);
  }

  parseInput(str: string): [Formula[], Formula] {
    console.log("*** parsing input");
    this.input = str;
    const parts = str.split('|=');
    if (parts.length > 2) {
      throw new Error("You can't use more than one turnstile.");
    }
    let premises: Formula[] = [];
    const conclusion = this.parseFormula(parts[parts.length - 1]);
    if ((conclusion as any).isArray) {
      throw new Error(`${parts[parts.length - 1]} looks like a list; use either conjunction or disjunction instead of the comma.`);
    }
    console.log("=== conclusion " + conclusion);
    if (parts.length === 2 && parts[0] !== '') {
      premises = this.parseFormula(parts[0]);
      if (!(premises as any).isArray) premises = [premises];
      console.log("=== premises: " + premises);
    }
    if (this.isModal) this.initModality();
    return [premises, conclusion];
  }

  parseFormula(str: string, boundVars: string[] = []): Formula {
    console.log(`parsing '${str}' (boundVars ${boundVars})`);

    if (!boundVars) {
      str = this.tidyFormula(str);
    }

    const temp = this.hideSubStringsInParens(str);
    let nstr = temp[0];
    const subStringsInParens = temp[1];
    console.log(`   nstr = '${nstr}'; `);

    if (nstr === '%0') {
      console.log("trying again without surrounding parens");
      return this.parseFormula(str.replace(/^\((.*)\)$/, "$1"), boundVars);
    }

    // Handle complex formulas (with connectives)
    const reTest = nstr.match(/,/) || nstr.match(/↔/) || nstr.match(/→/) || nstr.match(/∨/) || nstr.match(/∧/);
    if (reTest) {
      const op = reTest[0];
      console.log(`   string is complex (or list); main connective: ${op}; `);
      if (op === ',') nstr = nstr.replace(/,/g, '%split');
      else nstr = nstr.replace(op, "%split");
      
      // Restore substrings from parentheses
      for (let i = 0; i < subStringsInParens.length; i++) {
        nstr = nstr.replace(`%${i}`, subStringsInParens[i]);
      }

      const substrings = nstr.split("%split");
      if (!substrings[1]) {
        throw new Error(`argument missing for operator ${op} in ${str}.`);
      }
      console.log(`   substrings: ${substrings}`);
      const subFormulas: Formula[] = [];
      for (const substring of substrings) {
        subFormulas.push(this.parseFormula(substring, boundVars));
      }
      return new BinaryFormula(op, subFormulas[0], subFormulas[1]);
    }

    // Check for negation or modal operator
    const reNegOrModal = nstr.match(/^(¬|□|◇)/);
    if (reNegOrModal) {
      const op = reNegOrModal[1];
      const sub = this.parseFormula(str.substr(1), boundVars);
      if (op === '¬') return new NegatedFormula(sub);
      this.isModal = true;
      return new ModalFormula(op, sub);
    }

    // Handle quantified formula
    const reQuantifier = /^(∀|∃)([^\d\(\),%]\d*)/.exec(str);
    if (reQuantifier && reQuantifier.index === 0) {
      const quantifier = reQuantifier[1];
      const variable = reQuantifier[2];
      if (!str.substr(reQuantifier[0].length)) {
        throw new Error(`There is nothing in the scope of ${str}.`);
      }
      if (this.expressionType[variable] !== 'world variable') {
        this.registerExpression(variable, 'variable', 0);
      }
      boundVars.push(variable);
      this.isPropositional = false;
      const sub = this.parseFormula(str.substr(reQuantifier[0].length), boundVars);
      return new QuantifiedFormula(quantifier, variable, sub);
    }

    // Handle atomic formula
    const m = str.match(/[□◇∃∀¬]/);
    if (m) {
      throw new Error(`I don't understand '${m[0]}' in '${str}'. Missing operator?`);
    }

    return this.parseAtomicFormula(str);
  }

  parseAtomicFormula(str: string): Formula {
    const re = /^[\w$=]/.exec(str);
    if (re && re.index === 0) {
      console.log(`   string is atomic (predicate = '${re[0]}'); `);
      // Logic for parsing atomic formula...
      // Return appropriate AtomicFormula
    }
    throw new Error(`Parse Error.\n'${str}' is not a well-formed formula.`);
  }

  tidyFormula(str: string): string {
    str = str.replace(/\s/g, ''); // remove whitespace
    str = str.replace('[', '(').replace(']', ')');
    this.checkBalancedParentheses(str);
    str = str.replace(/\(([∀∃]\w\d*)\)/g, '$1');
    const m = str.match(/[^\w\d\(\)∀∃□◇♢∧↔∨¬→,=+\-*ξω$]/);
    if (m) throw new Error(`I don't understand the symbol '${m[0]}'.`);
    console.log(str);
    return str;
  }

  checkBalancedParentheses(str: string): void {
    const openings = str.split('(').length - 1;
    const closings = str.split(')').length - 1;
    if (openings !== closings) {
      throw new Error(`unbalanced parentheses: ${openings} opening parentheses, ${closings} closing.`);
    }
  }

  hideSubStringsInParens(str: string): [string, string[]] {
    const subStringsInParens: string[] = [];
    let parenDepth = 0;
    let storingAtIndex = -1;
    let nstr = '';
    for (let i = 0; i < str.length; i++) {
      if (str.charAt(i) === '(') {
        parenDepth++;
        if (parenDepth === 1) {
          storingAtIndex = subStringsInParens.length;
          subStringsInParens[storingAtIndex] = '';
          nstr += '%' + storingAtIndex;
        }
      }
      if (storingAtIndex === -1) nstr += str.charAt(i);
      else subStringsInParens[storingAtIndex] += str.charAt(i);
      if (str.charAt(i) === ')') {
        parenDepth--;
        if (parenDepth === 0) storingAtIndex = -1;
      }
    }
    return [nstr, subStringsInParens];
  }
}

