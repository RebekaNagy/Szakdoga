module Main where
import Verification
import Parser
import Preparation
import System.IO
import System.Environment

main :: IO String 
main = do
--    contents <- getArgs
    contents <- readFile "test2.p4"
    let result = parseString (contents)
    mapM print result
    putStrLn ""
    let out = mainConversion result ((initEnv, finalEnv), (initActions, initTables, initProg))
    let envs = fst out
    let prog = snd out
    let strprog = dataToString prog
    let verifyenv = (verifyP4 (fst envs) prog empSideCons 0)
    let compare = compareCalculatedWithFinal verifyenv (snd envs)
    putStrLn "Initial:"
    mapM print (fst envs)
    putStrLn ""
    putStrLn "Final:"
    print (snd envs)
    putStrLn ""
    putStrLn "Program:"
    print prog
    putStrLn ""
    putStrLn "VerifyEnvironment:"
    mapM print verifyenv
    putStrLn ""
    putStrLn "ComparedEnvs:"
    mapM print compare
    let stringo = envListToString compare ++ "&" ++ (finalEnvListToString (snd envs)) ++ "&" ++ (initEnvListToString (fst (envs)))    
    putStrLn ""
    print stringo
    return strprog

--ghc --make -shared calculation.hs