{-# LANGUAGE MonadComprehensions #-}
module Lambda.SystemF.Parser where

import Text.ParserCombinators.Parsec

import Lambda.SystemF.Term
import Lambda.CommonUtils

parseTerm :: String -> Either ParseError LambdaTerm
parseTerm = runParser lambdaTerm () ""

parseType :: String -> Either ParseError Type
parseType = runParser lambdaType () ""

lambdaType = lambdaTypeAtom `chainr1` (do { many (string " ");
                                            string "->";
                                            many (string " ");
                                            return (Arrow)
                                          })

lambdaTypeAtom = lambdaTVar
              <|> (try lambdaForAll)
              <|> (between (string "(")
                           (string ")")
                            lambdaType)
           -- <|> lambdaArrow
lambdaTVar = [TVar v | v <- typeVariableName]
--lambdaArrow = [Arrow t s | t <- lambdaType
--                         , _ <- string "->"
--                         , s <- lambdaType]
lambdaForAll = [ForAll v t | _ <- string "("
                           , v <- typeVariableName
                           , _ <- string ":*)"
                           , _ <- string "->"
                           , t <- lambdaType]

--lambdaTerm = lambdaVar <|> lambdaApp <|> lambdaTApp
--          <|> lambdaLam <|> lambdaTLam <|> paren
lambdaTerm = lambdaAtom `chainl1` (do { string " " <|> lambdaSym;
                                        return (App)
                                      })
lambdaAtom = lambdaVar
--          <|> try (lambdaTApp)
          <|> lambdaLambda
--          <|> try (lambdaLam) <|> try (lambdaTLam)
          <|> lambdaTApp
          <|> paren

lambdaVar = [Var v | v <- variableName]
lambdaApp = [App m n | m <- lambdaTerm, n <- lambdaTerm]
lambdaTApp = [TApp m t | m <- lambdaTLam
                       , _ <- many (string " ")
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

lambdaSym = string "\\"
paren = between (string "(") (string ")") lambdaTerm
