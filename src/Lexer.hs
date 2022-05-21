module Lexer
  ( lexer,
    Token (..),
  )
where

import Data.Char (isAlphaNum, isDigit)
import Data.Type.Bool (If)

data Token
  = OpenBrace
  | CloseBrace
  | OpenParenthesis
  | CloseParenthesis
  | SemiColon
  | Equality
  | KeywordInt
  | Return
  | If
  | Else
  | While
  | Identifier String
  | IntegerLiteral Int
  | InvalidToken
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
  | otherwise = InvalidToken : lexer rest
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
  "if" -> If : lexer suffix
  "else" -> Else : lexer suffix
  "while" -> While : lexer suffix
  _ -> Identifier prefix : lexer suffix
  where
    (prefix, suffix) = span isAlphaNum string
