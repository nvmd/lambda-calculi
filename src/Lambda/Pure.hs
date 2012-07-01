module Lambda.Pure where
import Data.List

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
-- causes *** Exception: y \in FV(y) --- use alpha-conversion needs to be used to prevent it

-- Lambda-term
type Sym = String
data LambdaTerm = Var Sym                    -- variable
                | App LambdaTerm LambdaTerm  -- application
                | Lam Sym LambdaTerm         -- abstraction
                deriving (Eq, Read)

instance Show LambdaTerm where
    show (Var sym)               = sym
    show (App (Var s1) (Var s2)) = s1 ++ s2
    show (App t1 t2)             = "(" ++ show t1 ++ ")(" ++ show t2 ++ ")"
    show (Lam sym t)             = "\\" ++ sym ++ "." ++ show t

freeVars :: LambdaTerm -> [Sym]
freeVars (Var v)   = [v]
freeVars (App p q) = freeVars p `union` freeVars q
freeVars (Lam v t) = freeVars t \\ [v]

-- M[x:=N], where x is free in M
-- substitute free occurrences of 'sym' in 'm' with 'n'
substitution :: LambdaTerm -> Sym -> LambdaTerm -> LambdaTerm
substitution m@(Var var)   sym n | var == sym  = n
                                 | otherwise   = m
substitution   (App p q)   sym n = App (substitution p sym n) (substitution q sym n)
substitution m@(Lam var p) sym n | var == sym            = m  -- 'sym' is bound - no substitution
                                 | var `elem` freeVars n = error (var ++ " \\in FV(" ++ show n ++ ")")
                                 | otherwise             = Lam var $ substitution p sym n   -- var /= sym

-- (\x.M)N = M[x:=N]
betaConversion :: LambdaTerm -> LambdaTerm
betaConversion (App (Lam x m) n) = substitution m x n
betaConversion m = m

betaReduction :: LambdaTerm -> LambdaTerm
betaReduction redex@(App (Lam x m) n) = betaReduction $ betaConversion redex
betaReduction m@(App p q) | r == m    = r               -- application can potentially become redex after...
                          | otherwise = betaReduction r -- ...beta-reduction of it's terms
                          where r = App (betaReduction p) (betaReduction q)
betaReduction (Lam v p) = Lam v $ betaReduction p
betaReduction m = m

alphaConversion :: LambdaTerm -> Sym -> Sym -> LambdaTerm
alphaConversion (Lam v m) x y | v /= x = Lam v $ alphaConversion m x y
                              | y `elem` freeVars m = error (y ++ " \\in FV(" ++ show m ++ ")")
                              | otherwise = Lam y $ substitution m x (Var y) -- v == x
alphaConversion (App p q) x y = App (alphaConversion p x y) (alphaConversion q x y)
alphaConversion m@(Var v) x y | v == x    = Var y
                              | otherwise = m

-- \lambda x.Mx = M, when x \not\in FV(M)
etaConversion :: LambdaTerm -> LambdaTerm
etaConversion n@(Lam x (App m (Var y))) | x == y && x `elem` (freeVars m) = n
                                        | otherwise                       = m

-- Combinators
isCombinator :: LambdaTerm -> Bool
isCombinator t = freeVars t == []

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
