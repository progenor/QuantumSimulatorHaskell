import QuantumSimulator
import System.Random
import System.Environment
import Control.Monad
import Data.Time.Clock
import Text.Printf

generateClassicalBits :: Int -> IO [Int]
generateClassicalBits n = do
    gen <- getStdGen
    let randomDoubles = randoms gen :: [Double]
    return $ take n $ map (\x -> if x < 0.5 then 0 else 1) randomDoubles

runQuantumSimulation :: Int -> String -> IO ()
runQuantumSimulation iterations outFile = do
    putStrLn $ "Running quantum simulation with " ++ show iterations ++ " iterations..."
    startTime <- getCurrentTime
    
    let q1 = qubitZero  
    let qSuper =  hGate q1
    putStrLn $ "Initial state: " ++ show qSuper
    results <- simulateMeasurements qSuper iterations
    
    let zeros = length $ filter (== 0) results
    let ones = length $ filter (== 1) results
    let zeroPercent = (100 * fromIntegral zeros / fromIntegral iterations)
    let onePercent = (100 * fromIntegral ones / fromIntegral iterations)
    
    endTime <- getCurrentTime
    let duration = diffUTCTime endTime startTime
    
    writeFile outFile $ 
        
        concatMap (\x -> show x ++ "\n") results
    
    putStrLn $ "Measured |0⟩: " ++ show zeros ++ " times (" ++ show zeroPercent ++ "%)"
    putStrLn $ "Measured |1⟩: " ++ show ones ++ " times (" ++ show onePercent ++ "%)"
    putStrLn $ "Duration: " ++ show duration

runClassicalSimulation :: Int -> String -> IO ()
runClassicalSimulation iterations outFile = do
    putStrLn $ "Running classical simulation with " ++ show iterations ++ " iterations..."
    startTime <- getCurrentTime
    
    results <- generateClassicalBits iterations
    
    let zeros = length $ filter (== 0) results
    let ones = length $ filter (== 1) results
    let zeroPercent = 100.0 * fromIntegral zeros / fromIntegral iterations :: Double
    let onePercent = 100.0 * fromIntegral ones / fromIntegral iterations :: Double
    
    endTime <- getCurrentTime
    let duration = diffUTCTime endTime startTime
    
    writeFile outFile $ 
        
        concatMap (\x -> show x ++ "\n") results
    
    putStrLn $ "Results written to " ++ outFile
    putStrLn $ "Generated 0: " ++ show zeros ++ " times (" ++ printf "%.2f" zeroPercent ++ "%)"
    putStrLn $ "Generated 1: " ++ show ones ++ " times (" ++ printf "%.2f" onePercent ++ "%)"

runComparisonSimulation :: Int -> String -> IO ()
runComparisonSimulation iterations outFile = do
    putStrLn $ "Running comparison simulation with " ++ show iterations ++ " iterations..."
    
    let q1 = hGate qubitZero  
    quantumResults <- simulateMeasurements q1 iterations
    let quantumZeros = length $ filter (== 0) quantumResults
    let quantumOnes = length $ filter (== 1) quantumResults
    
    classicalResults <- generateClassicalBits iterations
    let classicalZeros = length $ filter (== 0) classicalResults
    
    writeFile outFile "iteration,quantum,classical\n"
    appendFile outFile $ 
        concatMap (\(i, q, c) -> show i ++ "," ++ show q ++ "," ++ show c ++ "\n") 
                  (zip3 [1..iterations] quantumResults classicalResults)
    
    let quantumZeroPercent = 100.0 * fromIntegral quantumZeros / fromIntegral iterations :: Double
    let classicalZeroPercent = 100.0 * fromIntegral classicalZeros / fromIntegral iterations :: Double
    
    putStrLn $ "Comparison results written to " ++ outFile
    putStrLn $ "Quantum |0⟩: " ++ show quantumZeros ++ " times (" ++ 
               printf "%.2f" quantumZeroPercent ++ "%)"
    putStrLn $ "Classical 0: " ++ show classicalZeros ++ " times (" ++ 
               printf "%.2f" classicalZeroPercent ++ "%)"

printUsage :: IO ()
printUsage = putStrLn $ unlines
    [ "Usage: gen [OPTIONS]"
    , "Generate quantum and/or classical random bit simulations"
    , ""
    , "Options:"
    , "  --quantum N FILE    Run quantum simulation with N iterations, save to FILE"
    , "  --classical N FILE  Run classical simulation with N iterations, save to FILE"
    , "  --compare N FILE    Run both simulations with N iterations, save comparison to FILE"
    , "  --help              Show this help message"
    , ""
    , "Examples:"
    , "  gen --quantum 10000 quantum_results.txt"
    , "  gen --classical 10000 classical_results.txt"
    , "  gen --compare 10000 comparison.csv"
    ]

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--quantum", n, file] -> runQuantumSimulation (read n) file
        ["--classical", n, file] -> runClassicalSimulation (read n) file
        ["--compare", n, file] -> runComparisonSimulation (read n) file
        ["--help"] -> printUsage
        [] -> do
            results <- simulateMeasurements qubitZero 10000
            let zeros = length $ filter (== 0) results
            let ones = length $ filter (== 1) results
            let zeroPercent = 100.0 * fromIntegral zeros / 10000 :: Double
            let onePercent = 100.0 * fromIntegral ones / 10000 :: Double
            putStrLn $ "Measured |0⟩: " ++ show zeros ++ " times (" ++ printf "%.2f" zeroPercent ++ "%)"
            putStrLn $ "Measured |1⟩: " ++ show ones ++ " times (" ++ printf "%.2f" onePercent ++ "%)"
            putStrLn "Done"
        _ -> printUsage