module QuantumSimulator 
  ( -- * Qubit data type
    Qubit(..)
    -- * Qubit states
  , qubitZero
  , qubitOne
  , qubitPlus
  , qubitMinus
    -- * Quantum gates
  , normalize
  , xGate
  , zGate
  , hGate
    -- * Measurement functions
  , measure
  , probZero
  , probOne
  , simulateMeasurements
    -- * Utility functions
  , showQubit
  , printProbabilities
  , printMeasurementResults
  ) where

import Data.Complex
import System.Random (randomRIO)
import Control.Monad (replicateM)

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

-- | Print a simple text-based visualization of probabilities
printProbabilities :: String -> Qubit -> IO ()
printProbabilities title q = do
  let prob0 = probZero q
      prob1 = probOne q
      bar0 = replicate (round (prob0 * 50)) '█'
      bar1 = replicate (round (prob1 * 50)) '█'
  
  putStrLn $ "\n" ++ title ++ ":"
  putStrLn $ "|0⟩: " ++ bar0 ++ " " ++ show (prob0 * 100) ++ "%"
  putStrLn $ "|1⟩: " ++ bar1 ++ " " ++ show (prob1 * 100) ++ "%"

-- | Print a text-based visualization of measurement results
printMeasurementResults :: String -> [Int] -> IO ()
printMeasurementResults title results = do
  let total = fromIntegral $ length results
      zeros = fromIntegral $ length $ filter (== 0) results
      ones = fromIntegral $ length $ filter (== 1) results
      zeroPercent = 100 * zeros / total
      onePercent = 100 * ones / total
      bar0 = replicate (round (zeroPercent / 2)) '█'
      bar1 = replicate (round (onePercent / 2)) '█'
  
  putStrLn $ "\n" ++ title ++ ":"
  putStrLn $ "|0⟩: " ++ bar0 ++ " " ++ show zeroPercent ++ "% (" ++ show (round zeros) ++ " measurements)"
  putStrLn $ "|1⟩: " ++ bar1 ++ " " ++ show onePercent ++ "% (" ++ show (round ones) ++ " measurements)"