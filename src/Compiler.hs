{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Compiler (compile, pass1, pass2, pass3, AST (..))  where

import Data.List (elemIndex)
import qualified Data.List.Split as S
import qualified Data.Map.Strict as Map
import Text.Parsec
import Text.Parsec.Expr

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

compile :: String -> [String]
compile = pass3 . pass2 . pass1

pass1 :: String -> AST
pass1 s = go $ runParse (removeSpaces s)
  where
    go (Right ast) = ast
    runParse s = runParser (parseArgs *> spaces *> expression) [] "" s

pass2 :: AST -> AST
pass2 (Imm x) = Imm x
pass2 (Arg x) = Arg x
pass2 (Mul (Imm x) (Imm y)) = Imm (x * y)
pass2 (Div (Imm x) (Imm y)) = Imm (x `div` y)
pass2 (Add (Imm x) (Imm y)) = Imm (x + y)
pass2 (Sub (Imm x) (Imm y)) = Imm (x - y)
pass2 (Mul x y) = recursivePass2 (Mul) x y
pass2 (Div x y) = recursivePass2 (Div) x y
pass2 (Add x y) = recursivePass2 (Add) x y
pass2 (Sub x y) = recursivePass2 (Sub) x y

pass3 :: AST -> [String]
pass3 (Imm x) = ["IM " ++ show x]
pass3 (Arg x) = ["AR " ++ show x]
pass3 (Add x y) = commutativeOp "AD" x y
pass3 (Mul x y) = commutativeOp "MU" x y
pass3 (Sub x y) = nonCommutativeOp "SU" x y
pass3 (Div x y) = nonCommutativeOp "DI" x y

commutativeOp :: String -> AST -> AST -> [String]
commutativeOp op x y = pass3Operation op x y

nonCommutativeOp :: String -> AST -> AST -> [String]
nonCommutativeOp op x y
  | simple x = pass3Operation op y x
  | otherwise = pass3Operation op x y

pass3Operation :: String -> AST -> AST -> [String]
pass3Operation op x y = concat [pass3 x, pushOrSwap x, pass3 y, operate op x y]

pushOrSwap :: AST -> [String]
pushOrSwap (Imm _) = ["SW"]
pushOrSwap (Arg _) = ["SW"]
pushOrSwap _ = ["PU"]

operate :: String -> AST -> AST -> [String]
operate op x y
  | simple x && simple y = [op]
  | otherwise = ["SW", "PO", op]

simple :: AST -> Bool
simple (Imm _) = True
simple (Arg _) = True
simple _ = False

recursivePass2 :: (AST -> AST -> AST) -> AST -> AST -> AST
recursivePass2 op x y
  | collapsible nextPassX nextPassY = pass2 (op nextPassX nextPassY)
  | otherwise = op nextPassX nextPassY
  where
    nextPassX = pass2 x
    nextPassY = pass2 y

collapsible :: AST -> AST -> Bool
collapsible (Imm _) (Imm _) = True
collapsible _ _ = False

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
expression = buildExpressionParser operatorTable factor <?> "failed parsing expression"

operatorTable =
  [
    [op "/" (Div) AssocLeft, op "*" (Mul) AssocLeft],
    [op "+" (Add) AssocLeft, op "-" (Sub) AssocLeft]
  ]
  where op s f assoc = Infix (do{ string s; return f}) assoc

factor = braces expression <|> arg <|> number <?> "failed parsing factor"

braces = between (char '(') (char ')')

parseArgs = between (char '[') (char ']') argsList <?> "failed parsing arguments"

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

