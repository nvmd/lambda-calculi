{-# LANGUAGE MonadComprehensions #-}
module Lambda.Pure.Parser where

import Text.ParserCombinators.Parsec
import Lambda.Pure.Term

import Test.HUnit
import Text.ParserCombinators.Parsec.Error(ParseError,
											--Message, 
											errorMessages,
											--messageEq
											)

parseTerm :: String -> Either ParseError LambdaTerm
parseTerm = runParser lambdaTerm () ""

-- T: V | A | L | P
-- V: <latin letter + '\''>
-- A: TT
-- L: '\'V'.'T
-- P: '('T')'

--lambdaTerm :: GenParser Char st LambdaTerm
lambdaTerm = lambdaVar <|> lambdaLam <|> paren <|> lambdaApp

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


lambdaVarSimple = TestCase (assertEqual "variable"
					(Right $ Var "x")
					(runParser lambdaVar () "" "x"))
lambdaVarSimplePrime = TestCase (assertEqual "variable"
						(Right $ Var "x'")
						(runParser lambdaVar () "" "x'"))
lambdaVarSimplePrime2 = TestCase (assertEqual "variable"
						(Right $ Var "x''")
						(runParser lambdaVar () "" "x''"))


lambdaLamSimpleBound = TestCase (assertEqual "lambda-abstraction"
						(Right $ Lam "x" (Var "x"))
						(runParser lambdaLam () "" "\\x.x"))

lambdaLamSimpleUnbound = TestCase (assertEqual "lambda-abstraction"
						(Right $ Lam "x" (Var "y"))
						(runParser lambdaLam () "" "\\x.y"))

stubTest = TestCase (assertEqual "Unimplemented" True False)

lambdaVarTests = TestList [lambdaVarSimple,
							lambdaVarSimplePrime,
							lambdaVarSimplePrime2]
lambdaAppTests = TestList [TestLabel "" stubTest]
lambdaLamTests = TestList [TestLabel "" lambdaLamSimpleBound,
						   TestLabel "" lambdaLamSimpleUnbound]
lambdaTermTests = TestList [TestLabel "" stubTest]

tests = TestList [TestLabel "lambdaVar" lambdaVarTests,
				  TestLabel "lambdaApp" lambdaAppTests,
				  TestLabel "lambdaLam" lambdaLamTests,
				  TestLabel "lambdaTerm" lambdaTermTests]
runTests = runTestTT tests

instance Eq ParseError where
   a == b = errorMessages a == errorMessages b

--instance Eq Message where
--   (==) = messageEq
