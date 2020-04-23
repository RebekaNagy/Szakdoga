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
    let envs = fst out
    let prog = snd out
    let strprog = dataToString prog
    let verifyenv = (verifyP4 (fst envs) prog empSideCons)
    putStrLn "Initial:"
    mapM print (fst envs)
    putStrLn ""
    putStrLn "Final:"
    print (snd envs)
    putStrLn ""
    putStrLn "Program:"
    print prog
    putStrLn ""
    putStrLn "Program as string:"
    putStr strprog
    putStrLn "\n"
    putStrLn "VerifyEnvironment:"
    mapM print verifyenv
    return strprog

--ghc --make -shared calculation.hs