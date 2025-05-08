import { proxy } from 'valtio';
import { parse, tokenize } from './parser';
import { ASTNode } from './parsed';

type Store = {
  expression: string;
  astNode: ASTNode | undefined
}

export const store = proxy<Store>({
  expression: '',
  astNode: undefined
});

export function updateExpression(value: string) {
  store.expression = value;
  console.log('Input updated:', value);
}

export function startProof() {
  try {
    console.log('Starting proof for input:', store.expression);
    const tokens = tokenize(store.expression)
    const astNode = parse(tokens)
    store.astNode = astNode
    console.log('Parsing successful!', astNode)
  } catch (e) {
    console.error('Parsing error:', e);
    alert(e instanceof Error ? e.message : String(e));
  }
}
