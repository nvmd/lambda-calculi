{-# LANGUAGE MonadComprehensions #-}
module Lambda.Pure.Parser where

import Text.ParserCombinators.Parsec
import Lambda.Pure.Term

--parseTerm :: String -> Either ParseError LambdaTerm
--parseTerm _ = fail "Not implemented"

-- T: V | A | L | P
-- V: <latin letter + '\''>
-- A: TT
-- L: '\'V'.'T
-- P: '('T')'

--lambdaTerm :: GenParser Char st LambdaTerm
lambdaTerm = lambdaVar <|> lambdaApp <|> lambdaLam <|> paren

--lambdaVar :: GenParser Char st LambdaTerm
lambdaVar = [Var v | v <- variableName]

--lambdaApp :: GenParser Char st LambdaTerm
lambdaApp = [App m n | m <- lambdaTerm, n <- lambdaTerm]

--lambdaLam :: GenParser Char st LambdaTerm
lambdaLam = [Lam v t | _ <- string "\\"
                     , v <- variableName
                     , _ <- string "."
                     , t <- lambdaTerm]

paren = between (string "(") (string ")") lambdaTerm

variableName :: GenParser Char st String
variableName = [v:primes | v      <- letter
						 , primes <- many (char '\'')]
--variable = [x:xs | x <- lower, xs <- many alphaNum]
