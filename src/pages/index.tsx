'use client'
import { store } from '../calculator/store';
import InputField from '../components/InputField';
import ActionButtons from '../components/ActionButtons';
import { useSnapshot } from 'valtio';

export default function Index() {
  // UI initialization logic (like updating input on load)
  //useEffect(() => {
  //  // Simulating a page load where we populate the input field with the stored value
  //  store.updateFlaFieldValue('Initial input value');
  //}, []);
  const snapshot = useSnapshot(store);

  return (
    <div>
      <h1>Tree Proof Generator</h1>
      <InputField />
      <ActionButtons />
      <pre>{JSON.stringify(snapshot, null, 2)}</pre>
    </div>
  );
};
