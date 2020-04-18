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
    let strprog = programToString second
    let verifyenv = verifyP4 (fst first) (head second)
    putStrLn "Initial:"
    mapM print (fst first)
    putStrLn ""
    putStrLn "Final:"
    print (snd first)
    putStrLn ""
    putStrLn "Program:"
    mapM print second
    putStrLn ""
    putStrLn "Program as string:"
    putStr strprog
    putStrLn ""
    putStrLn "VerifyEnvironment:"
    mapM print verifyenv
    return strprog

--ghc --make -shared calculation.hs