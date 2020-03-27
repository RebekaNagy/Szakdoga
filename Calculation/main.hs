module Main (main) where
import Verification
import Parser
import Preparation
import System.IO

main = do
    contents <- readFile "file.txt"
    let result = parseString contents
    print result
    let out = mainConversion result ((initEnv, finalEnv), (initActions, initTables, initProg))
    let first = fst out
    let second = snd out
    putStrLn "Initial:"
    print (fst first)
    putStrLn ""
    putStrLn "Final:"
    print (snd first)
    putStrLn ""
    putStrLn "Program:"
    print second

