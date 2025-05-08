// ActionButtons.tsx
import React from 'react';
import { useSnapshot } from 'valtio';
import { store } from '../calculator/store';

const ActionButtons: React.FC = () => {
  const snap = useSnapshot(store);

  const handleStartProof = () => {
    const parsedInput = store.parseInput();
    if (parsedInput) {
      // Assuming parsedInput contains premises and conclusion
      const [premises, conclusion] = parsedInput;
      // Initialize prover here and perform actions based on parsed input
      console.log("Proof started with premises and conclusion:", premises, conclusion);
      // You can implement the prover logic here or call another function
    }
  };

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
      <button onClick={handleStartProof}>Start Proof</button>
    </div>
  );
};

export default ActionButtons;

