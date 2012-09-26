module Lambda.SystemF.Combinator where

import Lambda.SystemF.Term

-- standard combinators
-- id: (a:*) -> a -> a
-- id = \(a:*).\(x:a).x, id = \(a:*)(x:a).x
iComb :: LambdaTerm
iComb = TLam "a" $ Lam "x" (TVar "a") $ Var "x"

iCombType :: Type
iCombType = ForAll "a" $ Arrow (TVar "a") (TVar "a")
