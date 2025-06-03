module Main where

import QuantumSimulator

-- | Main function for executable
main :: IO ()
main = do
  let q0 = qubitZero
  
  putStrLn "======= Quantum Simulator ======="
  putStrLn $ "Initial state: " ++ showQubit q0
  putStrLn $ "Prob |0⟩: " ++ show (probZero q0)
  putStrLn $ "Prob |1⟩: " ++ show (probOne q0)
  
  -- Print initial state probabilities
  printProbabilities "Initial state probabilities" q0
  
  let q1 = hGate q0  -- Apply Hadamard to create superposition
  putStrLn $ "\nAfter Hadamard: " ++ showQubit q1
  putStrLn $ "Prob |0⟩: " ++ show (probZero q1)
  putStrLn $ "Prob |1⟩: " ++ show (probOne q1)
  
  -- Print Hadamard state probabilities
  printProbabilities "After Hadamard" q1
  
  -- Perform 1000 measurements and count results
  results <- simulateMeasurements q1 1000
  let zeros = length $ filter (== 0) results
  let ones = length $ filter (== 1) results
  
  putStrLn $ "\nMeasurement results from 1000 samples:"
  putStrLn $ "Measured |0⟩: " ++ show zeros ++ " times (" ++ show (100 * fromIntegral zeros / 1000) ++ "%)"
  putStrLn $ "Measured |1⟩: " ++ show ones ++ " times (" ++ show (100 * fromIntegral ones / 1000) ++ "%)"
  
  -- Print measurement results
  printMeasurementResults "Measurement results (1000 samples)" results
  
  -- Demonstrate X gate (NOT)
  let q2 = xGate q0
  putStrLn $ "\nAfter X gate on |0⟩: " ++ showQubit q2
  
  -- Print X gate state probabilities
  printProbabilities "After X gate on |0⟩" q2
  
  -- Demonstrate H then X then H
  let q3 = hGate $ xGate $ hGate q0
  putStrLn $ "\nH → X → H on |0⟩: " ++ showQubit q3
  
  -- Print HXH state probabilities
  printProbabilities "After H → X → H" q3