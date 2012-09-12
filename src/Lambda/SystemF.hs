module Lambda.SystemF where

import Lambda.SystemF.Term
import Lambda.SystemF.Parser
import Lambda.SystemF.Combinator
import Lambda.SystemF.TypeChecking
import Lambda.SystemF.Test

-- \lambda2, polymorphic typed, second-order typed, second-order polymorphic typed, system F

i_int = TApp iComb (TVar "Int")
i_int_3 = App (TApp iComb (TVar "Int")) (Var "3")
--checkTermType iComb emptyEnv (ForAll "a" $ Arrow (TVar "a") (TVar "a")) == True
--checkTermType iComb emptyEnv (ForAll "a" $ Arrow (TVar "x") (TVar "x")) == False
--checkTermType (betaReduction $ TApp iComb (TVar "Int"))
--                (extend "Int" (TVar "*") emptyEnv)
--                (Arrow (TVar "Int") (TVar "Int")) == True
-- checkTermType (TApp iComb (TVar "Int"))
--                 (extend "Int" (TVar "*") emptyEnv)
--                 (Arrow (TVar "Int") (TVar "Int")) == True
