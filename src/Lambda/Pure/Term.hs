module Lambda.Pure.Term where

import Data.List

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
