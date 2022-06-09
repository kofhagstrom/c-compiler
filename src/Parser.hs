module Parser (parseLexing) where

import Lexer (Token (..), TokenType (..))

data AST_Exp = AST_Constant Int deriving (Show)

data AST_Statement = AST_Return AST_Exp deriving (Show)

data AST_Function = AST_Function String AST_Statement deriving (Show)

data AST_Program = EmptyProgram | AST_Program [AST_Function] deriving (Show)

parseLexing :: [Token] -> AST_Program
parseLexing [] = EmptyProgram
parseLexing ((Token KeywordInt _) : (Token Identifier id ): (Token OpenParenthesis _) :(Token CloseParenthesis _) :(Token OpenBrace _) : (Token Return _) : (Token IntegerLiteral intlit) : (Token SemiColon _) : (Token CloseBrace _) : []) =
  AST_Program [AST_Function id (AST_Return (AST_Constant (read intlit)))]

parseLexing' :: [Token] -> AST_Program
parseLexing' [] = EmptyProgram
parseLexing' tokens = AST_Program (parseFunctions tokens)

parseFunctions :: [Token] -> [AST_Function]
parseFunctions [] = []
parseFunctions tokens = (fst funcs) : parseFunctions (snd funcs)
  where funcs = parseFunction tokens

parseFunction :: [Token] -> (AST_Function, [Token])
parseFunction tokens = (AST_Function "main" (AST_Return (AST_Constant 2)), tokens)

