import { Formula, AtomicFormula, BinaryFormula, NegatedFormula, QuantifiedFormula } from './formula-types';
import { Token, Tokenizer, TokenType } from './tokenizer';

export class Parser {
  private tokens: Token[] = [];
  private pos: number = 0;

  parseInput(input: string): [Formula[], Formula] {
    this.tokens = new Tokenizer().tokenize(input);
    this.pos = 0;

    const premises: Formula[] = [];
    if (this.peek().type === 'TURNSTILE') {
      this.consume('TURNSTILE');
    } else {
      while (this.peek().type !== 'TURNSTILE' && this.peek().type !== 'EOF') {
        premises.push(this.parseExpression());
        if (this.peek().type === 'COMMA') this.consume('COMMA');
      }
      this.consume('TURNSTILE');
    }

    const conclusion = this.parseExpression();
    return [premises, conclusion];
  }

  parse(input: string): Formula {
    this.tokens = new Tokenizer().tokenize(input);
    this.pos = 0;
    return this.parseExpression();
  }

  private parseExpression(): Formula {
    // chain binary parsers in order of precedence
    return this.parseBinary(['IFF'], () =>
      this.parseBinary(['IMPLIES'], () =>
        this.parseBinary(['OR'], () =>
          this.parseBinary(['AND'], () => this.parseUnary())
        )
      )
    );
  }

  private parseBinary(
    ops: TokenType[],
    next: () => Formula
  ): Formula {
    let left = next();
    while (ops.includes(this.peek().type)) {
      const op = this.consume().value;
      const right = next();
      left = new BinaryFormula(op, left, right);
    }
    return left;
  }

  private parseUnary(): Formula {
    const token = this.peek();
    if (token.type === 'NOT') {
      this.consume('NOT');
      return new NegatedFormula(this.parseUnary());
    }
    if (token.type === 'FORALL' || token.type === 'EXISTS') {
      const q = this.consume().value;
      const v = this.consume('SYMBOL').value;
      return new QuantifiedFormula(q, v, this.parseUnary());
    }
    return this.parsePrimary();
  }

  private parsePrimary(): Formula {
    const token = this.peek();
    if (token.type === 'SYMBOL') {
      return new AtomicFormula(this.consume('SYMBOL').value);
    }
    if (token.type === 'LPAREN') {
      this.consume('LPAREN');
      const expr = this.parseExpression();
      this.consume('RPAREN');
      return expr;
    }
    throw new Error(`Unexpected token: ${token.value}`);
  }

  private peek(): Token {
    return this.tokens[this.pos] || { type: 'EOF', value: '' };
  }

  private consume(expected?: TokenType): Token {
    const token = this.peek();
    if (expected && token.type !== expected) {
      throw new Error(`Expected ${expected}, got ${token.type}`);
    }
    this.pos++;
    return token;
  }
}
