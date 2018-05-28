module Compiler (pass1, AST (..))  where

data AST = Imm Int
         | Arg Int
         | Add AST AST
         | Sub AST AST
         | Mul AST AST
         | Div AST AST
         deriving (Eq, Show)

pass1 :: String -> AST
pass1 x = Imm $ read (last (words x))
