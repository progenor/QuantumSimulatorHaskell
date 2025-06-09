module QuantumSimulator 
  ( Qubit(..)
  , qubitZero
  , qubitOne
  , qubitPlus
  , qubitMinus
  , normalize
  , xGate
  , zGate
  , hGate
  , measure
  , probZero
  , probOne
  , simulateMeasurements
  , showQubit
  , printProbabilities
  , printMeasurementResults
  ) where

import Data.Complex
import System.Random (randomRIO)
import Control.Monad (replicateM)

data Qubit = Qubit
  { amplitudeZero :: Complex Double
  , amplitudeOne  :: Complex Double
  } deriving (Eq, Show)

qubitZero :: Qubit
qubitZero = Qubit { amplitudeZero = 1 :+ 0, amplitudeOne = 0 :+ 0 }

qubitOne :: Qubit
qubitOne = Qubit { amplitudeZero = 0 :+ 0, amplitudeOne = 1 :+ 0 }

qubitPlus :: Qubit
qubitPlus = Qubit
  { amplitudeZero = 1/sqrt 2 :+ 0
  , amplitudeOne = 1/sqrt 2 :+ 0
  }

qubitMinus :: Qubit
qubitMinus = Qubit
  { amplitudeZero = 1/sqrt 2 :+ 0
  , amplitudeOne = (-1/sqrt 2) :+ 0
  }

normalize :: Qubit -> Qubit
normalize q =
  let norm = sqrt (magnitude (amplitudeZero q)^2 + magnitude (amplitudeOne q)^2)
  in Qubit
      { amplitudeZero = amplitudeZero q / (norm :+ 0)
      , amplitudeOne = amplitudeOne q / (norm :+ 0)
      }

xGate :: Qubit -> Qubit
xGate q = Qubit
  { amplitudeZero = amplitudeOne q
  , amplitudeOne = amplitudeZero q
  }

zGate :: Qubit -> Qubit
zGate q = Qubit
  { amplitudeZero = amplitudeZero q
  , amplitudeOne = negate (amplitudeOne q)
  }

hGate :: Qubit -> Qubit
hGate q = normalize $ Qubit
  { amplitudeZero = amplitudeZero q + amplitudeOne q
  , amplitudeOne = amplitudeZero q - amplitudeOne q
  }

measure :: Qubit -> IO (Int, Qubit)
measure q = do
  let probZero = magnitude (amplitudeZero q)^2
  r <- randomRIO (0, 1) :: IO Double
  if r <= probZero
    then return (0, qubitZero)
    else return (1, qubitOne)

probZero :: Qubit -> Double
probZero q = magnitude (amplitudeZero q)^2

probOne :: Qubit -> Double
probOne q = magnitude (amplitudeOne q)^2

showQubit :: Qubit -> String
showQubit q =
  let a = amplitudeZero q
      b = amplitudeOne q
      showComplex (r :+ i)
        | i == 0 = show r
        | otherwise = "(" ++ show r ++ " + " ++ show i ++ "i)"
  in showComplex a ++ "|0⟩ + " ++ showComplex b ++ "|1⟩"

simulateMeasurements :: Qubit -> Int -> IO [Int]
simulateMeasurements q n = replicateM n $ do
  (result, _) <- measure q
  return result

printProbabilities :: String -> Qubit -> IO ()
printProbabilities title q = do
  let prob0 = probZero q
      prob1 = probOne q
      bar0 = replicate (round (prob0 * 50)) '█'
      bar1 = replicate (round (prob1 * 50)) '█'
  
  putStrLn $ "\n" ++ title ++ ":"
  putStrLn $ "|0⟩: " ++ bar0 ++ " " ++ show (prob0 * 100) ++ "%"
  putStrLn $ "|1⟩: " ++ bar1 ++ " " ++ show (prob1 * 100) ++ "%"

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