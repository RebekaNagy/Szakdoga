module Main where
import Verification
import Parser
import Preparation
import System.IO
import System.Environment

main :: IO String 
main = do
--    contents <- getArgs
    contents <- readFile "file.p4"
    let result = parseString (contents)
    print result
    putStrLn ""
    let out = mainConversion result ((initEnv, finalEnv), (initActions, initTables, initProg))
    let first = fst out
    let second = snd out
    let strprog = programToString second
    putStrLn "Initial:"
    print (fst first)
    putStrLn ""
    putStrLn "Final:"
    print (snd first)
    putStrLn ""
    putStrLn "Program:"
    print second
    putStrLn ""
    putStrLn "Program as string:"
    putStr strprog
    return strprog

--ghc --make -shared calculation.hs