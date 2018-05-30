module Compiler (pass1, AST (..), Token (..))  where

import Data.List.Split

data AST = Imm Int
         | Arg Int
         | Add AST AST
         | Sub AST AST
         | Mul AST AST
         | Div AST AST
         deriving (Eq, Show)

data Token = TChar Char
           | TInt Int
           | TStr String
           deriving (Eq, Show)

pass1 :: String -> AST
pass1 = pass1' . tokenise . last . (splitOn "]")

pass1' :: [Token] -> AST
pass1' (TInt x : []) = Imm x
pass1' (TInt x : TChar '-' : ts) = Sub (Imm x) (pass1' ts)
pass1' (TInt x : TChar '+' : ts) = Add (Imm x) (pass1' ts)
pass1' _ = undefined

tokenToAst :: Token -> AST
tokenToAst (TInt t) = Imm t
tokenToAst _ = Imm 1

tokenise :: String -> [Token]
tokenise  [] = []
tokenise xxs@(c:cs)
  | c `elem` "-+*/()[]" = TChar c : tokenise cs
  | not (null i) = TInt (read i) : tokenise is
  | not (null s) = TStr s : tokenise ss
  | otherwise = tokenise cs
  where
    (i, is) = span (`elem` digit) xxs
    (s, ss) = span (`elem` alpha) xxs

alpha, digit :: String
alpha = ['a'..'s'] ++ ['A'..'z']
digit = ['0'..'9']

