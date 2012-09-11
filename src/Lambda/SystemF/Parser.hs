{-# LANGUAGE MonadComprehensions #-}
module Lambda.SystemF.Parser where

import Text.ParserCombinators.Parsec

import Lambda.SystemF.Term
import Lambda.CommonUtils

parseTerm :: String -> Either ParseError LambdaTerm
parseTerm _ = fail "Not implemented"

parseType :: String -> Either ParseError Type
parseType _ = fail "Not implemented"

lambdaType = lambdaTVar <|> lambdaArrow <|> lambdaForAll
lambdaTVar = [TVar v | v <- typeVariableName]
lambdaArrow = [Arrow t s | t <- lambdaType
						             , _ <- string "->"
						             , s <- lambdaType]
lambdaForAll = [ForAll v t | _ <- string "("
						               , v <- typeVariableName
						               , _ <- string ":*)"
                           , _ <- string "->"
                           , t <- lambdaType]

lambdaTerm = lambdaVar <|> lambdaApp <|> lambdaTApp 
          <|> lambdaLam <|> lambdaTLam <|> paren
lambdaVar = [Var v | v <- variableName]
lambdaApp = [App m n | m <- lambdaTerm, n <- lambdaTerm]
lambdaTApp = [TApp m t | m <- lambdaTerm
                       , t <- lambdaType]
lambdaLam = [Lam v t m | _ <- lambdaSym
                       , v <- variableName
                       , _ <- string ":"
                       , t <- lambdaType
                       , _ <- string "."
                       , m <- lambdaTerm]
lambdaTLam = [TLam v t | _ <- lambdaSym
                       , v <- typeVariableName
                       , _ <- string "."
                       , t <- lambdaTerm]

lambdaSym = string "\\"
paren = between (string "(") (string ")") lambdaTerm
