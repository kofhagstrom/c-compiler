module Main (main) where

import Lexer (lexer)
import Parser (parseLexing)

main :: IO ()
main = do
  filePath <- getLine
  contents <- readFile filePath
  print . parseLexing . lexer . unwords . words $ contents
