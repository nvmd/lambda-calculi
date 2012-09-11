module Lambda.Pure where

import Lambda.Pure.Term
import Lambda.Pure.Combinator
import Lambda.Pure.Parser
import Lambda.Pure.Test

-- f = (\x.\y.x)(\z.z)
f = App (Lam "x" $ Lam "y" $ Var "x") (Lam "z" $ Var "z")
f' = App (Lam "x" $ Lam "y" $ Var "x") (Lam "z" $ App (Var "z") (Var "a"))
-- yx(\x.x) = (yx)(\x.x)
yx = App (App (Var "y") (Var "x")) (Lam "x" $ Var "x")

kiComb = App kComb iComb
iikstarComb = App (App iComb iComb) kstarComb
-- kiComb == iikstarComb == \y.\x.x == kstarComb
-- substitution kComb "x" iComb == \x.\y.x == kComb -- because 'x' is bound in kComb
i_kiComb = App iComb $ App kComb iComb

-- eta-conversion
-- \x.xx -> \x.xx
--etaConversion $ Lam "x" (App (Var "x") (Var "x")) == Lam "x" (App (Var "x") (Var "x"))
-- \x.yx -> y
--etaConversion $ Lam "x" (App (Var "y") (Var "x")) == Var "y"

-- alpha-conversion
-- e.g., substitution yxy "x" (Var "y")
yxy = Lam "y" $ App (Var "x") (Var "y")
-- causes *** Exception: y \in FV(y) --- alpha-conversion needs to be used to prevent it
