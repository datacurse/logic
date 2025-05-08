'use client'
import React from 'react';
import { useSnapshot } from 'valtio';
import { store, updateInput } from '../calculator/store';

export default function InputField() {
  const snap = useSnapshot(store);
  
  function handleInputChange(e: React.ChangeEvent<HTMLInputElement>) {
    updateInput(e.target.value);
  }
  
  return (
    <div>
      <input
        type="text"
        value={snap.inputField}
        onChange={handleInputChange}
        placeholder="Enter your logical formula..."
        className="w-full p-2 border border-gray-300 rounded"
      />
    </div>
  );
}
