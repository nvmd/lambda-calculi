{-# LANGUAGE MonadComprehensions #-}
module Lambda.Pure.Parser where

import Text.ParserCombinators.Parsec

import Lambda.Pure.Term
import Lambda.CommonUtils

parseTerm :: String -> Either ParseError LambdaTerm
parseTerm = runParser lambdaTerm () ""

-- T: V | A | L | P
-- V: <latin letter + '\''>
-- A: TT
-- L: '\'V'.'T
-- P: '('T')'

--lambdaTerm :: GenParser Char st LambdaTerm
lambdaTerm = lambdaAtom `chainl1` (do {string " " <|> lambdaSym; return (App)})

lambdaAtom = lambdaVar <|> lambdaLam <|> paren -- <|> lambdaApp

--lambdaVar :: GenParser Char st LambdaTerm
lambdaVar = [Var v | v <- variableName]

--lambdaApp :: GenParser Char st LambdaTerm
--lambdaApp = [App m n | m <- lambdaTerm, n <- lambdaTerm]

--lambdaLam :: GenParser Char st LambdaTerm
lambdaLam = [Lam v t | _ <- lambdaSym
                     , v <- variableName
                     , _ <- string "."
                     , t <- lambdaTerm]

lambdaSym = string "\\"
paren = between (string "(") (string ")") lambdaTerm
