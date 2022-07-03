module Main (main) where

import Lexer (lexer)
import Parser (parseLexing)

lexFileContent :: String -> IO ()
lexFileContent = print . parseLexing . lexer . unwords . words

main :: IO ()
main = do
    filePath <- getLine
    fileContents <- readFile filePath
    lexFileContent fileContents

main' :: IO()
main' = getLine >>= readFile >>= lexFileContent