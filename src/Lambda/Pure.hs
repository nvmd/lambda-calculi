module Lambda.Pure where
import Data.List

-- Определите тип данных для описания лямбда-терма чистого лямбда-исчисления.
-- Реализуйте функции подстановки, одношаговой редукции (в нормальном порядке)
-- и многошаговой редукции к нормальной форме.

-- f = (\x.\y.x)(\z.z)
f = App (Lam "x" $ Lam "y" $ Var "x") (Lam "z" $ Var "z")
f' = App (Lam "x" $ Lam "y" $ Var "x") (Lam "z" $ App (Var "z") (Var "a"))
-- yx(\x.x) = (yx)(\x.x)
yx = App (App (Var "y") (Var "x")) (Lam "x" $ Var "x")

-- Lambda-term
type Sym = String
data LambdaTerm = Var Sym                    -- variable
                | App LambdaTerm LambdaTerm  -- application
                | Lam Sym LambdaTerm         -- abstraction
                deriving (Eq, Read, Show)

freeVars :: LambdaTerm -> [Sym]
freeVars (Var v)     = [v]
freeVars (App p q) = freeVars p `union` freeVars q
freeVars (Lam v t)   = freeVars t \\ [v]

-- M[x:=N], where x is free in M
-- substitute free occurrences of 'sym' in 'm' with 'n'
substitution :: LambdaTerm -> Sym -> LambdaTerm -> LambdaTerm
substitution m@(Var var)   sym n | var == sym  = n
                                 | otherwise   = m
substitution   (App p q)   sym n = App (substitution p sym n) (substitution q sym n)
substitution m@(Lam var m) sym n | var == sym  = m  -- 'sym' is bound - no substitution
                                 | otherwise   = substitution m sym n

-- (\x.M)N = M[x:=N]
betaReduction :: LambdaTerm -> LambdaTerm
betaReduction (App (Lam x m) n) = substitution m x n
betaReduction m = m

-- Combinators
isCombinator :: LambdaTerm -> Bool
isCombinator t = freeVars t == []

-- standard combinators
iComb :: LambdaTerm
iComb = Lam "x" $ Var "x"

kComb :: LambdaTerm
kComb = Lam "x" $ Lam "y" $ Var "x"

sComb :: LambdaTerm
sComb = Lam "x" $ Lam "y" $ Lam "z" $ App (App (Var "x") (Var "y")) (App (Var "y") (Var "z"))

hI :: a -> a
hI x = x

hK :: a -> b -> a
hK x _ = x

