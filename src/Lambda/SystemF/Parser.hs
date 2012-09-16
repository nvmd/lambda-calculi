{-# LANGUAGE MonadComprehensions #-}
module Lambda.SystemF.Parser where

import Text.ParserCombinators.Parsec

import Lambda.SystemF.Term
import Lambda.CommonUtils
import Lambda.SystemF.TypeParser

parseTerm :: String -> Either ParseError LambdaTerm
parseTerm = runParser lambdaTerm () ""

--lambdaTerm = lambdaVar <|> lambdaApp <|> lambdaTApp
--          <|> lambdaLam <|> lambdaTLam <|> paren
lambdaTerm = lambdaAtom `chainl1` (do { string " " <|> lambdaSym;
                                        return (App)
                                      })
lambdaAtom = lambdaVar
          <|> try (lambdaTLamApp)
          <|> lambdaLambda
          <|> paren

lambdaVar = [Var v | v <- variableName]
lambdaApp = [App m n | m <- lambdaTerm, n <- lambdaTerm]
lambdaTApp = [TApp m t | m <- lambdaTLam
                       , _ <- spaces
                       , t <- lambdaType]

lambdaLambda = do {
                    lambdaLambdaPrefix; -- Lam and TLam common prefix parser
                    -- Lam and TLam suffixes parsers
                    try (lambdaLamSuffix) <|> try (lambdaTLamSuffix);
                }
lambdaLambdaPrefix = do { lambdaSym;
                          string "(";
                      }
lambdaLamSuffix  = [Lam v t m | v <- variableName
                              , _ <- string ":"
                              , t <- lambdaType
                              , _ <- string ")."
                              , m <- lambdaTerm]
lambdaTLamSuffix = [TLam v t | v <- typeVariableName
                             , _ <- string ":*)."
                             , t <- lambdaTerm]
lambdaTLam = do { lambdaLambdaPrefix; lambdaTLamSuffix; }

lambdaTLamApp = [TApp (TLam v m) t | _ <- string "("
                                   , _ <- lambdaSym
                                   , _ <- string "("
                                   , v <- typeVariableName
                                   , _ <- string ":*)."
                                   , m <- lambdaTerm
                                   , _ <- string ")"
                                   , t <- lambdaType]

lambdaSym = string "\\"
paren = between (string "(") (string ")") lambdaTerm
