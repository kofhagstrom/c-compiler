module Main (main) where

import Lib (lexer)

main :: IO ()
main = do
  filePath <- getLine
  contents <- readFile filePath
  print . lexer . unwords . words $ contents
