module Lambda.Pure.Test where

import Lambda.Pure.Term
import Lambda.Pure.Parser

import Text.ParserCombinators.Parsec

import Test.HUnit
import Text.ParserCombinators.Parsec.Error(ParseError,
											--Message, 
											errorMessages,
											--messageEq
											)

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

lambdaExprT0 = TestCase (assertEqual "lambdaExprT0"
						(Right $ (App (Var "x") (Var "y")))
						(parseTerm "x y"))
lambdaExprT00 = TestCase (assertEqual "lambdaExprT00"
						(Right $ (App (Var "x") (Var "y")))
						(parseTerm "x (y)"))
lambdaExprT00' = TestCase (assertEqual "lambdaExprT00'"
						(Right $ (App (Var "x") (Var "y")))
						(parseTerm "(x) (y)"))
lambdaExprT00'' = TestCase (assertEqual "lambdaExprT00''"
						(Right $ (App (Var "x") (Var "y")))
						(parseTerm "((x) (y))"))
lambdaExprT000 = TestCase (assertEqual "lambdaExprT000"
                        (Right $ (App (Var "x") (Var "y")))
                        (parseTerm "(x)(y)"))
lambdaExprT0' = TestCase (assertEqual "lambdaExprT0'"
						(Right $ (App (Var "x") (App (Var "z") (Var "y"))))
						(parseTerm "x (z y)"))
lambdaExprT1 = TestCase (assertEqual "lambdaExprT1"
						(Right $ Lam "x" (App (Var "y") (Lam "z" (App (Var "z") (Var "yx")))))
						(parseTerm "\\x.(y \\z.z yx)"))
lambdaExprT2 = TestCase (assertEqual "lambdaExprT2"
						(Right $ Lam "x" (App (Var "y") (Lam "z" (App (App (Var "z") (Var "y")) (Var "x")))))
						(parseTerm "\\x.(y \\z.z y x)"))
lambdaExprT3 = TestCase (assertEqual "lambdaExprT3"
						(Right $ Lam "x" (App (Var "y") (Lam "z" (App (Var "z") (App (Var "y") (Var "x"))))))
						(parseTerm "\\x.(y \\z.z (y x))"))
lambdaExprT4 = TestCase (assertEqual "lambdaExprT4"
						(Right $ App (Lam "x" (Lam "y" (Lam "z" (Var "z")))) (Lam "u" (Var "v")))
						(parseTerm "(\\x.\\y.\\z.z) \\u.v"))
lambdaExprT5 = TestCase (assertEqual "lambdaExprT5"
						(Right $ Var "x")
						(parseTerm "x"))
lambdaExprT6 = TestCase (assertEqual "lambdaExprT6"
						(Right $ Var "x")
						(parseTerm "x "))
lambdaExprT7 = TestCase (assertEqual "lambdaExprT7"
						(Right $ Var "x")
						(parseTerm "  x"))
lambdaExprT8 = TestCase (assertEqual "lambdaExprT8"
						(Right $ App (Var "x") (Var "y"))
						(parseTerm "x  y"))
lambdaExprT9 = TestCase (assertEqual "lambdaExprT9"
						(Right $ App (Var "x") (Var "y"))
						(parseTerm " x  y"))

stubTest = TestCase (assertEqual "Stub test case" True True)

lambdaVarTests = TestList [lambdaVarSimple,
							lambdaVarSimplePrime,
							lambdaVarSimplePrime2]
lambdaAppTests = TestList [TestLabel "" stubTest]
lambdaLamTests = TestList [TestLabel "" lambdaLamSimpleBound,
						   TestLabel "" lambdaLamSimpleUnbound]
lambdaTermTests = TestList [TestLabel "" stubTest,
							lambdaExprT0, lambdaExprT00,
							lambdaExprT00', lambdaExprT00'',
							lambdaExprT000,
							lambdaExprT0',
							lambdaExprT1, lambdaExprT2,
							lambdaExprT3, lambdaExprT4,
							lambdaExprT5, lambdaExprT6,
							lambdaExprT7, lambdaExprT8,
							lambdaExprT9]

tests = TestList [TestLabel "lambdaVar" lambdaVarTests,
				  TestLabel "lambdaApp" lambdaAppTests,
				  TestLabel "lambdaLam" lambdaLamTests,
				  TestLabel "lambdaTerm" lambdaTermTests]
runTests = runTestTT tests

instance Eq ParseError where
   a == b = errorMessages a == errorMessages b

--instance Eq Message where
--   (==) = messageEq
