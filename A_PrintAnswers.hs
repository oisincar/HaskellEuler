import           System.Exit
import           System.Process
import           System.IO

main = do
  -- Print as soon as we can, no buffering.
  hSetBuffering stdout NoBuffering

  files <- readProcess "bash" ["-c", "find Euler*hs"] "."
  mapM_ buildF (lines files)

buildF file = do
  putStr $ file ++ " | Building | "
  exitCode <- system $ "ghc " ++ file ++ " -v0 -O2"

  putStr "Running | "
  runF (takeOff 3 file) exitCode
    where takeOff n str = take (length str - n) str

runF file exitC
  | exitC == ExitSuccess = ("Ans: " ++) <$> readProcess ("./" ++ file) [] [] >>= putStr
  | otherwise            = putStrLn "Can't run: Build Failed"
