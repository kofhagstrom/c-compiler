module Lexer
  ( lexer,
    Token (..),
    TokenType (..),
  )
where

import Data.Char (isAlphaNum, isDigit)
import Data.Type.Bool (If)

data TokenType = OpenBrace
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
  | Identifier
  | IntegerLiteral
  | InvalidToken deriving (Show)

data Token = Token {tokenType::TokenType, valueString :: String} deriving (Show)

lexer :: String -> [Token]
lexer string
  | null string = []
  | char == ' ' = lexer rest
  | char == '{' = (Token OpenBrace "{"): lexer rest
  | char == '}' = (Token CloseBrace "}"): lexer rest
  | char == '(' = (Token OpenParenthesis "(") : lexer rest
  | char == ')' = (Token CloseParenthesis ")") : lexer rest
  | char == ';' = (Token SemiColon ";") : lexer rest
  | char == '=' = (Token Equality "=") : lexer rest
  | isDigit char = lexIntegerLiteral string
  | isAlphaNum char = lexIdentifier string
  | otherwise = (Token InvalidToken string): lexer rest
  where
    (char : rest) = string

lexIntegerLiteral :: String -> [Token]
lexIntegerLiteral string = (Token IntegerLiteral prefix) : lexer suffix
  where
    (prefix, suffix) = span isAlphaNum string

lexIdentifier :: String -> [Token]
lexIdentifier string = case prefix of
  "return" -> (Token Return "return") : lexer suffix
  "int" -> (Token KeywordInt "int"): lexer suffix
  "if" -> (Token If "if") : lexer suffix
  "else" -> (Token Else "else") : lexer suffix
  "while" -> (Token While "while") : lexer suffix
  _ -> (Token Identifier prefix) : lexer suffix
  where
    (prefix, suffix) = span isAlphaNum string
