import { proxy } from 'valtio';
import { Formula } from './formula-types';
import { Parser } from './parser';

type Store = {
  inputField: string;
  parsedInput: [Formula[], Formula] | null;
}

export const store = proxy<Store>({
  inputField: '',
  parsedInput: null
});

export function updateInput(value: string) {
  store.inputField = value;
  console.log('Input updated:', value);
}

export function startProof() {
  const parser = new Parser();
  try {
    console.log('Starting proof for input:', store.inputField);
    store.parsedInput = parser.parseInput(store.inputField);
    console.log('Parsing successful!', store.parsedInput);
  } catch (e) {
    console.error('Parsing error:', e);
    alert(e instanceof Error ? e.message : String(e));
  }
}
