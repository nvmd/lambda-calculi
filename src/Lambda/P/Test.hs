module Lambda.P.Test where

import Lambda.P.Term

import Control.Monad (mzero)
import Test.HUnit

tx = (Var "x")
tg = (Context [("x", (Var "Int")), ("Int", Type)])

runTests = runTestTT tests

tests = test [ "context" ~: ctxTests
             , "typing" ~: typeTests
             ]

ctxTests = test
  [ "ctx1" ~: True ~=?
     checkCtx (emptyCtx :: Context LambdaPTerm)
  , "ctx2" ~: True ~=?
     checkCtx (Context [("Int", Type)])
  , "ctx3" ~: False ~=?
     checkCtx (Context [("x", (Var "Int"))])
  , "ctx4" ~: True ~=?
     checkCtx (Context [("x", (Var "Int")), ("Int", Type)])
  , "ctx5" ~: False ~=?
     checkCtx (Context [("Int", Type), ("x", (Var "Int"))])
  ]
typeTests = test
  [ "type1" ~: mzero ~=?
     doType (Var "x") emptyCtx
  , "type2" ~: return Type ~=?
     doType (Var "Int") (Context [("Int", Type)])
  , "type3" ~: mzero ~=?
     doType (Var "x") (Context [("x", (Var "Int"))])
  , "type4" ~: return (Var "Int") ~=?
     doType (Var "x") (Context [("x", (Var "Int")), ("Int", Type)])
  ]
