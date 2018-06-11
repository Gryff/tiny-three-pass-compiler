{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Compiler (pass1, AST (..))  where

import Data.List (elemIndex)
import qualified Data.List.Split as S
import qualified Data.Map.Strict as Map
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Expr
import Text.Parsec.Token

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
    arguments = (pass1arguments . tokenise . tail . head . (S.splitOn "]")) x
    body = (tokenise . last . (S.splitOn "]")) x

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

toAst :: String -> AST
toAst s = pass1' Map.empty (tokenise s)

expression :: Monad m => ParsecT String [String] m AST
expression = buildExpressionParser operatorTable factor <?> "expression parse failed"

operatorTable =
  [
    [op "/" (Div) AssocLeft],
    [op "*" (Mul) AssocLeft],
    [op "+" (Add) AssocLeft],
    [op "-" (Sub) AssocLeft]
  ]
  where op s f assoc = Infix (do{ string s; return f}) assoc

factor = mybraces <|> arg <|> number <?> "failed at factor"

mybraces = toAst <$> between (char '(') (char ')') (many1 (digit <|> oneOf "-+*/"))

mybrackets = between (char '[') (char ']') argsList <?> "failed at braces"

argsList = do
  args <- spaces *> sepBy (many1 letter) spaces
  setState args
  return $ Imm 1

arg :: Monad m => ParsecT String [String] m AST
arg = do
  arg <- many1 letter
  argsList <- getState
  return $ idx (elemIndex arg argsList)
  where
    idx (Just x) = Arg x
    idx Nothing = error "this should not happen"

number = digitToToken <$> (many1 digit) <?> "failed at number"

digitToToken = ((\t -> tokenToAst t Map.empty) . TInt . read)

tokenise :: String -> [Token]
tokenise  [] = []
tokenise xxs@(c:cs)
  | c `elem` "-+*/()[]" = TChar c : tokenise cs
  | not (null i) = TInt (read i) : tokenise is
  | not (null s) = TStr s : tokenise ss
  | otherwise = tokenise cs
  where
    (i, is) = span (`elem` digits) xxs
    (s, ss) = span (`elem` alpha) xxs

alpha, digits :: String
alpha = ['a'..'z'] ++ ['A'..'z']
digits = ['0'..'9']

