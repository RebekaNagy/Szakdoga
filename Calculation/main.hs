module Main (main) where
import Verification
import ParserOld
import Parser
import System.IO

main = do
    contents <- readFile "exam.txt"
    let contents = strGress1
    putStrLn contents

