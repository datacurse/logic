export class Tokenizer {
  private specs: [RegExp, TokenType | null][] = [
    [/^\|=/, 'TURNSTILE'],
    [/^(<->|↔)/, 'IFF'],
    [/^(->|→)/, 'IMPLIES'],
    [/^(∀)/, 'FORALL'],
    [/^(∃)/, 'EXISTS'],
    [/^(¬|!|~)/, 'NOT'],
    [/^(∧|&)/, 'AND'],
    [/^(∨|\|)/, 'OR'],
    [/^\(/, 'LPAREN'],
    [/^\)/, 'RPAREN'],
    [/^,/, 'COMMA'],
    [/^[A-Za-z_][A-Za-z0-9_]*/, 'SYMBOL'],
    [/^\s+/, null],         // skip whitespace
  ];

  tokenize(input: string): Token[] {
    const tokens: Token[] = [];
    let str = input;
    while (str.length > 0) {
      let matched = false;
      for (const [regex, type] of this.specs) {
        const m = str.match(regex);
        if (m) {
          matched = true;
          const text = m[0];
          if (type) {
            tokens.push({ type, value: text });
          }
          str = str.slice(text.length);
          break;
        }
      }
      if (!matched) {
        throw new Error(`Unexpected token near '${str}'`);
      }
    }
    return tokens;
  }
}
