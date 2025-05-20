module Main where

import Data.Complex
import System.Random (randomRIO)
import Control.Monad (replicateM)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

-- | Represents a qubit as a superposition of |0⟩ and |1⟩ states
-- The complex numbers are the probability amplitudes
-- (alpha)|0⟩ + (beta)|1⟩ where |alpha|² + |beta|² = 1
data Qubit = Qubit
  { amplitudeZero :: Complex Double  -- Amplitude for |0⟩ state
  , amplitudeOne  :: Complex Double  -- Amplitude for |1⟩ state
  } deriving (Eq, Show)

-- | Creates a qubit in the |0⟩ state
qubitZero :: Qubit
qubitZero = Qubit { amplitudeZero = 1 :+ 0, amplitudeOne = 0 :+ 0 }

-- | Creates a qubit in the |1⟩ state
qubitOne :: Qubit
qubitOne = Qubit { amplitudeZero = 0 :+ 0, amplitudeOne = 1 :+ 0 }

-- | Creates a qubit in the |+⟩ state (equal superposition)
qubitPlus :: Qubit
qubitPlus = Qubit
  { amplitudeZero = 1/sqrt 2 :+ 0
  , amplitudeOne = 1/sqrt 2 :+ 0
  }

-- | Creates a qubit in the |-⟩ state
qubitMinus :: Qubit
qubitMinus = Qubit
  { amplitudeZero = 1/sqrt 2 :+ 0
  , amplitudeOne = (-1/sqrt 2) :+ 0
  }

-- | Ensure a qubit's state is normalized (amplitudes squared sum to 1)
normalize :: Qubit -> Qubit
normalize q =
  let norm = sqrt (magnitude (amplitudeZero q)^2 + magnitude (amplitudeOne q)^2)
  in Qubit
      { amplitudeZero = amplitudeZero q / (norm :+ 0)
      , amplitudeOne = amplitudeOne q / (norm :+ 0)
      }

-- | X gate (NOT gate) - flips the qubit
xGate :: Qubit -> Qubit
xGate q = Qubit
  { amplitudeZero = amplitudeOne q
  , amplitudeOne = amplitudeZero q
  }

-- | Z gate - adds a phase of -1 to the |1⟩ state
zGate :: Qubit -> Qubit
zGate q = Qubit
  { amplitudeZero = amplitudeZero q
  , amplitudeOne = negate (amplitudeOne q)
  }

-- | Hadamard gate - creates superposition
hGate :: Qubit -> Qubit
hGate q = normalize $ Qubit
  { amplitudeZero = amplitudeZero q + amplitudeOne q
  , amplitudeOne = amplitudeZero q - amplitudeOne q
  }

-- | Measures a qubit, collapsing its state
-- Returns a tuple of (measured value, collapsed qubit)
measure :: Qubit -> IO (Int, Qubit)
measure q = do
  let probZero = magnitude (amplitudeZero q)^2
  r <- randomRIO (0, 1) :: IO Double
  if r <= probZero
    then return (0, qubitZero)
    else return (1, qubitOne)

-- | Computes the probability of measuring |0⟩
probZero :: Qubit -> Double
probZero q = magnitude (amplitudeZero q)^2

-- | Computes the probability of measuring |1⟩
probOne :: Qubit -> Double
probOne q = magnitude (amplitudeOne q)^2

-- | Pretty print a qubit state
showQubit :: Qubit -> String
showQubit q =
  let a = amplitudeZero q
      b = amplitudeOne q
      showComplex (r :+ i)
        | i == 0 = show r
        | otherwise = "(" ++ show r ++ " + " ++ show i ++ "i)"
  in showComplex a ++ "|0⟩ + " ++ showComplex b ++ "|1⟩"

-- | Run multiple measurements on the same qubit state
simulateMeasurements :: Qubit -> Int -> IO [Int]
simulateMeasurements q n = replicateM n $ do
  (result, _) <- measure q
  return result

-- | Create a bar chart showing the probabilities of |0⟩ and |1⟩ states
plotQubitProbabilities :: Qubit -> FilePath -> IO ()
plotQubitProbabilities q fileName = do
  let prob0 = probZero q
      prob1 = probOne q
      
  toFile def fileName $ do
    layout_title .= "Qubit State Probabilities"
    layout_y_axis . laxis_generate .= scaledAxis def (0, 1.1)
    
    plot (bars ["Probabilities"] [("|0⟩", [prob0]), ("|1⟩", [prob1])])

-- | Create a bar chart showing the results of multiple measurements
plotMeasurementResults :: [Int] -> FilePath -> IO ()
plotMeasurementResults results fileName = do
  let total = fromIntegral $ length results
      zeros = fromIntegral $ length $ filter (== 0) results
      ones = fromIntegral $ length $ filter (== 1) results
      zeroPercent = 100 * zeros / total
      onePercent = 100 * ones / total
      
  toFile def fileName $ do
    layout_title .= "Measurement Results"
    layout_y_axis . laxis_generate .= scaledAxis def (0, 110)
    
    plot (bars ["Results (%)"] [("|0⟩", [zeroPercent]), ("|1⟩", [onePercent])])

-- | Main function for executable
main :: IO ()
main = do
  let q0 = qubitZero
  
  putStrLn $ "Initial state: " ++ showQubit q0
  putStrLn $ "Prob |0⟩: " ++ show (probZero q0)
  putStrLn $ "Prob |1⟩: " ++ show (probOne q0)
  
  -- Plot initial state probabilities
  plotQubitProbabilities q0 "initial_state.svg"
  putStrLn "Initial state probabilities plotted to 'initial_state.svg'"
  
  let q1 = hGate q0  -- Apply Hadamard to create superposition
  putStrLn $ "\nAfter Hadamard: " ++ showQubit q1
  putStrLn $ "Prob |0⟩: " ++ show (probZero q1)
  putStrLn $ "Prob |1⟩: " ++ show (probOne q1)
  
  -- Plot Hadamard state probabilities
  plotQubitProbabilities q1 "hadamard_state.svg"
  putStrLn "Hadamard state probabilities plotted to 'hadamard_state.svg'"
  
  -- Perform 1000 measurements and count results
  results <- simulateMeasurements q1 1000
  let zeros = length $ filter (== 0) results
  let ones = length $ filter (== 1) results
  
  putStrLn $ "\nMeasurement results from 1000 samples:"
  putStrLn $ "Measured |0⟩: " ++ show zeros ++ " times (" ++ show (100 * fromIntegral zeros / 1000) ++ "%)"
  putStrLn $ "Measured |1⟩: " ++ show ones ++ " times (" ++ show (100 * fromIntegral ones / 1000) ++ "%)"
  
  -- Plot measurement results
  plotMeasurementResults results "measurement_results.svg"
  putStrLn "Measurement results plotted to 'measurement_results.svg'"
  
  -- Demonstrate X gate (NOT)
  let q2 = xGate q0
  putStrLn $ "\nAfter X gate on |0⟩: " ++ showQubit q2
  
  -- Plot X gate state probabilities
  plotQubitProbabilities q2 "x_gate_state.svg"
  putStrLn "X gate state probabilities plotted to 'x_gate_state.svg'"
  
  -- Demonstrate H then X then H
  let q3 = hGate $ xGate $ hGate q0
  putStrLn $ "\nH → X → H on |0⟩: " ++ showQubit q3
  
  -- Plot HXH state probabilities
  plotQubitProbabilities q3 "hxh_state.svg"
  putStrLn "HXH state probabilities plotted to 'hxh_state.svg'"