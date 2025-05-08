// ActionButtons.tsx
import React from 'react';
import { useSnapshot } from 'valtio';
import { startProof, store } from '../calculator/store';

const ActionButtons: React.FC = () => {
  const snap = useSnapshot(store);

  const handleToggleStatus = () => {
    const statusButton = document.getElementById("statusbtn");
    if (statusButton) {
      if (statusButton.innerText === 'stop') {
        statusButton.innerText = 'continue';
        // Implement prover.stop() logic here
      } else {
        statusButton.innerText = 'stop';
        // Implement prover.start() logic here
      }
    }
  };

  return (
    <div>
      <button id="statusbtn" onClick={handleToggleStatus}>stop</button>
      <button onClick={startProof} className='bg-green-500'>Start Proof</button>
    </div>
  );
};

export default ActionButtons;

