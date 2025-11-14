{-
  A basic calculator for arithmetic expressions
  Based on the example in Chapter 8 of "Programming in Haskell"
  by Graham Hutton.

  Pedro Vasconcelos, 2025
-}
module Main where


import Parsing
import Data.Char
--import Text.Parsec.Token (GenTokenParser(identifier))



type Env = [(String, Integer)]
--
-- a data type for expressions
-- made up from integer numbers, +,*,/,%,-, variables and expressions
--
data Expr = Num Integer
          | Add Expr Expr
          | Mul Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
          | Rem Expr Expr
          | Var String 
          | Assign String Expr -- Variables = expression 
          deriving Show

-- a recursive evaluator for expressions
--
eval :: Env -> Expr -> (Integer, Env) 

eval env (Num n) = (n, env)


eval env (Var x) = 
  case lookup x env of
    Just v -> (v, env)
    Nothing -> error ("undefined variable: " ++ x) 

eval env (Add e1 e2) = 
  let (v1, env1) = eval env e1
      (v2, env2) = eval env1 e2
  in (v1 + v2, env2) 


eval env (Assign name expr) = 
  let (val, env') = eval env expr
      newEnv = (name, val) : filter ((/= name) . fst) env'
  in (val, newEnv)



eval env (Mul e1 e2) = 
  let (v1, env1) = eval env e1
      (v2, env2) = eval env1 e2
  in (v1 * v2, env2)



eval env (Sub e1 e2) = 
  let (v1, env1) = eval env e1
      (v2, env2) = eval env1 e2
  in (v1 - v2, env2)


eval env (Div e1 e2) = 
  let (v1, env1) = eval env e1
      (v2, env2) = eval env1 e2
  in (v1 `div` v2, env2)


eval env (Rem e1 e2) = 
  let (v1, env1) = eval env e1
      (v2, env2) = eval env1 e2
  in (v1 `mod` v2, env2)

-- | a parser for expressions
-- Grammar rules:
--
-- expr ::= term exprCont
-- exprCont ::= '+' term exprCont | '-' term exprCont | epsilon

-- term ::= factor termCont
-- termCont ::= '*' factor termCont | '/' factor termCont | '%' factor termCont | epsilon
-- factor ::= variable | natural | '(' expr ')'
-- command ::= variable '=' expr | expr 




expr :: Parser Expr
expr = do t <- term
          exprCont t


command :: Parser Expr 
command =
        (do v <- variable
            char '='
            e <- expr 
            return (Assign v e))
  <|> expr

variable :: Parser String 
variable = (do 
  c <- satisfy isAlpha
  cs <- many (satisfy isAlphaNum)
  return (c:cs))
exprCont :: Expr -> Parser Expr
exprCont acc = do char '+'
                  t <- term
                  exprCont (Add acc t)
              <|> do char '-'
                     t <- term 
                     exprCont (Sub acc t)
               <|> return acc
              
term :: Parser Expr
term = do f <- factor
          termCont f

termCont :: Expr -> Parser Expr
termCont acc =  do char '*'
                   f <- factor  
                   termCont (Mul acc f)
            <|> do char '/'
                   f <- factor 
                   termCont (Div acc f)
            <|> do char '%'
                   f <- factor 
                   termCont (Rem acc f)  
            <|> return acc

factor :: Parser Expr
factor =
      do n <- natural
         return (Num n)
  <|> do v <- variable
         return (Var v)
  <|> do char '('
         e <- expr
         char ')'
         return e

             

natural :: Parser Integer
natural = do xs <- many1 (satisfy isDigit)
             return (read xs)

----------------------------------------------------------------             
  
main :: IO ()
main
  = do txt <- getContents
       calculator [] (lines txt)

-- | read-eval-print loop
calculator :: Env -> [String] -> IO ()
calculator _ [] = return () 
calculator env (l:ls) = 
  case parse command l of 
    [ (tree, "")] -> 
      let (val, newEnv) = eval env tree 
      in do print val 
            calculator newEnv ls
    _ -> do putStrLn "parse error; try again"
            calculator env ls



-- | evaluate a single expression
-- evaluate :: String -> String
-- evaluate txt
--  = case parse expr txt of
--      [ (tree, "") ] ->  show (eval tree)
--    _ -> "parse error; try again"



-- execute aceita uma lista de pares (env) e uma expressão 
execute :: Env -> String -> (String, Env)
execute env line = 
  case parse command line of
    [(tree, "")] -> 
      let (val, newEnv) = eval env tree
      in (show val, newEnv)
    _ -> ("parse error; try again", env)


