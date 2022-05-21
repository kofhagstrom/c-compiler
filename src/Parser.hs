module Parser (parseLexing) where

import Lexer (Token (..))

data AST_Exp = AST_Constant Int deriving (Show)

data AST_Statement = AST_Return AST_Exp deriving (Show)

data AST_Function = AST_Function String AST_Statement deriving (Show)

data AST_Program = Empty | AST_Program AST_Function deriving (Show)

parseLexing :: [Token] -> AST_Program
parseLexing [] = Empty
parseLexing (KeywordInt : Identifier id : OpenParenthesis : CloseParenthesis : OpenBrace : Return : IntegerLiteral intlit : SemiColon : CloseBrace : []) =
  AST_Program (AST_Function id (AST_Return (AST_Constant intlit)))
