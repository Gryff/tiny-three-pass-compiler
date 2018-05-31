module Compiler (pass1, AST (..))  where

import Data.List.Split
import qualified Data.Map.Strict as Map

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
           deriving (Eq, Ord, Show)

pass1 :: String -> AST
pass1 x = pass1' arguments body
  where
    arguments = (pass1arguments . tokenise . tail . head . (splitOn "]")) x
    body = (tokenise . last . (splitOn "]")) x

pass1arguments :: [Token] -> Map.Map Token AST
pass1arguments ts = foldl storeArg Map.empty (zip [0..] ts)
  where
    storeArg tMap (idx, TStr t) = Map.insert (TStr t) (Arg idx) tMap
    storeArg _ _ = Map.empty

pass1' :: Map.Map Token AST -> [Token] -> AST
pass1' _ (TInt x : []) = Imm x
pass1' args (x : TChar '-' : ts) = Sub (tokenToAst x args) (pass1' args ts)
pass1' args (x : TChar '+' : ts) = Add (tokenToAst x args) (pass1' args ts)
pass1' args (x : TChar '*' : ts) = Mul (tokenToAst x args) (pass1' args ts)
pass1' args (x : TChar '/' : ts) = Div (tokenToAst x args) (pass1' args ts)
pass1' args (var : []) = args Map.! var
pass1' _ _ = undefined

tokenToAst :: Token -> Map.Map Token AST -> AST
tokenToAst (TInt x) _ = Imm x
tokenToAst (TStr s) args = args Map.! (TStr s)
tokenToAst _ _ = undefined

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

