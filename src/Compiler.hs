{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Compiler (pass1, pass2, AST (..))  where

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
pass1 s = go $ runParse (removeSpaces s)
  where
    go (Right ast) = ast
    go (Left err) = error "something happened and I can't get the error"
    runParse s = runParser (mybrackets *> spaces *> expression) [] "" s

pass2 :: AST -> AST
pass2 ast = Imm 10

removeSpaces s = args ++ "]" ++ (filter (not . (`elem` " ")) body)
  where
    argsAndBody = S.splitOn "]" s
    args = argsAndBody !! 0
    body = argsAndBody !! 1

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
    [op "/" (Div) AssocLeft, op "*" (Mul) AssocLeft],
    [op "+" (Add) AssocLeft, op "-" (Sub) AssocLeft]
  ]
  where op s f assoc = Infix (do{ string s; return f}) assoc

factor = mybraces expression <|> arg <|> number <?> "failed at values"

mybraces = between (char '(') (char ')')

mybrackets = between (char '[') (char ']') argsList <?> "failed at parsing arguments"

argsList = do
  spaces
  args <- (many1 letter) `endBy` spaces
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

