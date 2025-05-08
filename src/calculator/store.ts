import { proxy } from 'valtio';

type Store = {
  inputField: string;
}

export const store = proxy<Store>({
  inputField: '',
});

export function updateInput(value: string) {
  store.inputField = value;
}
