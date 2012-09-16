{-# LANGUAGE MonadComprehensions #-}
module Lambda.CommonUtils where

import Text.ParserCombinators.Parsec

variableName :: GenParser Char st String
variableName = [v : s ++ primes | v      <- lower
                                , s      <- many alphaNum
                                , primes <- many (char '\'')]

typeVariableName :: GenParser Char st String
typeVariableName = [h : t ++ primes | h      <- lower
                                    , t      <- many alphaNum
                                    , primes <- many (char '\'')]

typeName :: GenParser Char st String
typeName = [h : t | h <- upper
                  , t <- many alphaNum]
