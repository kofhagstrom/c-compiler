module Lib
  ( lexer,
  )
where

import Data.Char (isAlphaNum, isDigit)

data Token
  = OpenBrace
  | CloseBrace
  | OpenParenthesis
  | CloseParenthesis
  | SemiColon
  | Equality
  | KeywordInt
  | Return
  | Identifier String
  | IntegerLiteral Int
  deriving (Eq, Show)

lexer :: String -> [Token]
lexer string
  | null string = []
  | char == ' ' = lexer rest
  | char == '{' = OpenBrace : lexer rest
  | char == '}' = CloseBrace : lexer rest
  | char == '(' = OpenParenthesis : lexer rest
  | char == ')' = CloseParenthesis : lexer rest
  | char == ';' = SemiColon : lexer rest
  | char == '=' = Equality : lexer rest
  | isDigit char = lexIntegerLiteral string
  | isAlphaNum char = lexIdentifier string
  where
    (char : rest) = string

lexIntegerLiteral :: String -> [Token]
lexIntegerLiteral string = IntegerLiteral (read prefix :: Int) : lexer suffix
  where
    (prefix, suffix) = span isAlphaNum string

lexIdentifier :: String -> [Token]
lexIdentifier string = case prefix of
  "return" -> Return : lexer suffix
  "int" -> KeywordInt : lexer suffix
  _ -> Identifier prefix : lexer suffix
  where
    (prefix, suffix) = span isAlphaNum string
