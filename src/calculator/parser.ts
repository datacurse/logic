import { ASTNode } from "./parsed";

// Tokenizer: Tokenizes the expression string into a list of tokens
export function tokenize(expression: string): string[] {
  // Add \|= before individual | and =
  // Change [A-Za-z]+ to capture individual characters for predicates like Fxy
  const regex = /\(|\)|∀|∃|□|→|↔|∧|∨|¬|\|=|[A-Za-z]|\d+|[=(),;|]/g;
  return expression.match(regex) || [];
}

// Recursive Descent Parser: Parses the token list into an AST
export function parse(tokens: string[]): ASTNode {
  let position = 0;
  
  function peek() {
    return position < tokens.length ? tokens[position] : null;
  }
  
  function consume() {
    return position < tokens.length ? tokens[position++] : null;
  }
  
  // Parse the entire logical expression, including possible entailment
  function parseLogic(): ASTNode {
    // First parse the left side (possibly a comma-separated list)
    const leftSide = parseSequent();
    
    // Check if we have an entailment operator
    if (position < tokens.length && tokens[position] === "|=") {
      consume(); // Skip |=
      // Parse the right side
      const rightSide = parseExpression();
      return {
        type: "operator",
        symbol: "|=",
        children: [leftSide, rightSide]
      };
    }
    
    return leftSide;
  }
  
  // Parse comma-separated expressions
  function parseSequent(): ASTNode {
    const expressions: ASTNode[] = [];
    expressions.push(parseExpression());
    
    // Keep collecting expressions separated by commas
    while (position < tokens.length && tokens[position] === ",") {
      consume(); // Skip comma
      expressions.push(parseExpression());
    }
    
    // If there's only one expression, return it directly
    if (expressions.length === 1) {
      return expressions[0];
    }
    
    // Otherwise, create a listing node
    return {
      type: "operator",
      symbol: "listing",
      children: expressions
    };
  }
  
  function parseExpression(): ASTNode {
    let node = parseTerm();
    
    while (position < tokens.length && 
           (tokens[position] === "∨" || tokens[position] === "∧")) {
      const operator = consume();
      const rightNode = parseTerm();
      node = {
        type: "operator",
        symbol: operator,
        children: [node, rightNode]
      };
    }
    
    return node;
  }
  
  function parseTerm(): ASTNode {
    let node = parseFactor();
    
    while (position < tokens.length && 
           (tokens[position] === "→" || tokens[position] === "↔")) {
      const operator = consume();
      const rightNode = parseFactor();
      node = {
        type: "operator",
        symbol: operator,
        children: [node, rightNode]
      };
    }
    
    return node;
  }
  
  function parseFactor(): ASTNode {
    if (position >= tokens.length) {
      throw new Error("Unexpected end of input");
    }
    
    const token = consume();
    
    if (token === "(") {
      const node = parseLogic(); // Start from the top level inside parentheses
      if (consume() !== ")") {
        throw new Error("Expected closing parenthesis");
      }
      return node;
    }
    
    if (token === "¬") {
      const rightNode = parseFactor();
      return {
        type: "operator",
        symbol: "¬",
        children: [rightNode]
      };
    }
    
    if (token === "∀" || token === "∃") {
      // Get the variable for the quantifier
      const variable = consume();
      if (!/[A-Za-z]/.test(variable)) {
        throw new Error("Expected variable after quantifier");
      }
      
      // Parse the body of the quantifier (which might contain other quantifiers)
      const bodyNode = parseFactor();
      
      return {
        type: "operator",
        symbol: token,
        children: [
          { type: "variable", symbol: variable },
          bodyNode
        ]
      };
    }
    
    if (token === "□") {
      const innerNode = parseFactor();
      return {
        type: "operator",
        symbol: "□",
        children: [innerNode]
      };
    }
    
    // Handle predicates (e.g., Fxy)
    if (/[A-Za-z]/.test(token)) {
      // Check if next tokens are variables
      const predicate = token;
      const args = [];
      
      // Collect variables until we hit a non-variable
      while (position < tokens.length && /[A-Za-z]/.test(tokens[position])) {
        args.push({ type: "variable", symbol: consume() });
      }
      
      // If no args, it's just an atomic proposition
      if (args.length === 0) {
        return { type: "atomic", symbol: predicate };
      }
      
      // Otherwise, it's a predicate with arguments
      return {
        type: "predicate",
        symbol: predicate,
        children: args
      };
    }
    
    if (/\d+/.test(token)) {
      return { type: "atomic", symbol: token };
    }
    
    throw new Error("Unexpected token: " + token);
  }
  
  // Start parsing from the top level
  return parseLogic();
}
