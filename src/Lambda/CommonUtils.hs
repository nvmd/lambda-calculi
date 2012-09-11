{-# LANGUAGE MonadComprehensions #-}
module Lambda.CommonUtils where

import Text.ParserCombinators.Parsec

variableName :: GenParser Char st String
--variableName = [v:primes | v      <- letter
--						 , primes <- many (char '\'')]
variableName = [v:s++primes | v      <- letter
                            , s      <- many alphaNum
						    , primes <- many (char '\'')]
--variable = [x:xs | x <- lower, xs <- many alphaNum]

typeVariableName :: GenParser Char st String
typeVariableName = [v:primes | v      <- letter
                             , primes <- many (char '\'')]
