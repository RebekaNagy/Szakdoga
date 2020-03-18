module Main (main) where
import Verification
import Parser
import System.IO

main = do
    contents <- readFile "exam.txt"
    let contents = strControl
    putStrLn contents