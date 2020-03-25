module Main (main) where
import Verification
import Parser
import System.IO

main = do
    contents <- readFile "file.txt"
    let result = parseString contents
    print result

