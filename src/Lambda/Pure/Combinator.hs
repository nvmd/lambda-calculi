module Lambda.Pure.Combinator where

import Lambda.Pure.Term

-- standard combinators
iComb :: LambdaTerm
iComb = Lam "x" $ Var "x"

kComb :: LambdaTerm
kComb = Lam "x" $ Lam "y" $ Var "x"

kstarComb :: LambdaTerm
kstarComb = Lam "x" $ Lam "y" $ Var "y"

sComb :: LambdaTerm
sComb = Lam "x" $ Lam "y" $ Lam "z" $ App (App (Var "x") (Var "z")) (App (Var "y") (Var "z"))

hI :: a -> a
hI x = x

hK :: a -> b -> a
hK x _ = x
