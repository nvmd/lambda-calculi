module Lambda.SystemF.Test where

import Lambda.SystemF.Term
import Lambda.SystemF.Parser

import Text.ParserCombinators.Parsec

import Test.HUnit
import Text.ParserCombinators.Parsec.Error(ParseError,
                                            --Message,
                                            errorMessages,
                                            --messageEq
                                            )


lambdaTypeT1 = TestCase (assertEqual "lambdaTypeT1"
                        (Right $ Arrow (TVar "a") (TVar "b"))
                        (parseType "a->b"))
lambdaTypeT2 = TestCase (assertEqual "lambdaTypeT2"
                        (Right $ Arrow (TVar "a") (TVar "b"))
                        (parseType "a -> b"))
lambdaTypeT3 = TestCase (assertEqual "lambdaTypeT3"
                        (Right $ Arrow (TVar "a") (Arrow (TVar "b") (TVar "c")))
                        (parseType "a->b->c"))
lambdaTypeT4 = TestCase (assertEqual "lambdaTypeT4"
                        (Right $ Arrow (TVar "a") (Arrow (TVar "b") (TVar "c")))
                        (parseType "a -> b -> c"))
-- id: (a:*)->a->a
lambdaTypeT5 = TestCase (assertEqual "lambdaTypeT5"
                        (Right $ ForAll "a" (Arrow (TVar "a") (TVar "a")))
                        (parseType "(a:*)->a->a"))

-- id = \(a:*).\(x:a).x
lambdaTermT1 = TestCase (assertEqual "lambdaTermT1"
                        (Right $ TLam "a" (Lam "x" (TVar "a") (Var "x")))
                        (parseTerm "\\(a:*).\\(x:a).x"))
lambdaTermT2 = TestCase (assertEqual "lambdaTermT2"
                        (Right $ Lam "x" (TVar "a") (Var "x"))
                        (parseTerm "\\(x:a).x"))
lambdaTermT3 = TestCase (assertEqual "lambdaTermT3"
                        (Right $ App (Var "x") (Var "y"))
                        (parseTerm "x y"))
lambdaTermT1' = TestCase (assertEqual "lambdaTermT1'"
                        (Right $ TLam "a" (Lam "x" (TVar "a") (Var "x")))
                        (runParser lambdaLambda () "" "\\(a:*).\\(x:a).x"))

stubTest = TestCase (assertEqual "Stub test case" True False)

lambdaTermTests = TestList [TestLabel "" stubTest,
                            lambdaTermT1, lambdaTermT2,
                            lambdaTermT3,
                            lambdaTermT1']

lambdaTypeTests = TestList [TestLabel "" stubTest,
                            lambdaTypeT1, lambdaTypeT2,
                            lambdaTypeT3, lambdaTypeT4,
                            lambdaTypeT5]

tests = TestList [TestLabel "lambdaTerm" lambdaTermTests,
                    TestLabel "lambdaType" lambdaTypeTests]

runTests = runTestTT tests

instance Eq ParseError where
   a == b = errorMessages a == errorMessages b

--instance Eq Message where
--   (==) = messageEq
