{-# LANGUAGE MonadComprehensions #-}
module Lambda.SystemF.TypeParser where

import Text.ParserCombinators.Parsec

import Lambda.SystemF.Term
import Lambda.CommonUtils

parseType :: String -> Either ParseError Type
parseType = runParser lambdaType () ""

lambdaType = lambdaTypeAtom `chainr1` (do { spaces;
                                            string "->";
                                            spaces;
                                            return (Arrow)
                                          })

lambdaTypeAtom = lambdaTVar
              <|> lambdaTVarType
              <|> (try lambdaForAll)
              <|> (between (string "(")
                           (string ")")
                            lambdaType)
           -- <|> lambdaArrow
lambdaTVarType = [TVar v | v <- typeName]
lambdaTVar = [TVar v | v <- typeVariableName]
--lambdaArrow = [Arrow t s | t <- lambdaType
--                         , _ <- string "->"
--                         , s <- lambdaType]
lambdaForAll = [ForAll v t | _ <- string "("
                           , v <- typeVariableName
                           , _ <- string ":*)"
                           , _ <- spaces
                           , _ <- string "->"
                           , _ <- spaces
                           , t <- lambdaType]
