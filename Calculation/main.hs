module Main where
import Verification
import Parser
import Preparation
import System.IO
import System.Environment

main :: IO String 
main = do
--    contents <- getArgs
    contents <- readFile "file.txt"
    let result = parseString (contents)
    mapM print result
    putStrLn ""
    let out = mainConversion result ((initEnv, finalEnv), (initActions, initTables, initProg))
    let first = fst out
    let second = snd out
    let strprog = dataToString second
    let verifyenv = (verifyP4 (fst first) second empSideCons)
    putStrLn "Initial:"
    mapM print (fst first)
    putStrLn ""
    putStrLn "Final:"
    print (snd first)
    putStrLn ""
    putStrLn "Program:"
    print second
    putStrLn ""
    putStrLn "Program as string:"
    putStr strprog
    putStrLn "\n"
    putStrLn "VerifyEnvironment:"
    mapM print (snd verifyenv)
    return strprog

--ghc --make -shared calculation.hs