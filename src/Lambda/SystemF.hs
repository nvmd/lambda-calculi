module Lambda.SystemF where

-- \lambda2, polymorphic typed, second-order typed, second-order polymorphic typed, system F

-- à la Church
type Sym = String
data LambdaTerm = Var Sym                    -- variable
                | App LambdaTerm LambdaTerm  -- application
                | Lam Sym Type LambdaTerm    -- abstraction (\Sym:Type.LambdaTerm)
                | TLam Sym LambdaTerm        -- abstraction by type variable (\Sym.LambdaTerm)
                deriving (Eq, Read, Show)

-- Types (same for \lambda2-Curry and \lambda2-Church)
-- \mathbb{T} = \mathbb{V} | \mathbb{T} \rightarrow \mathbb{T} | \forall \mathbb{V}.\mathbb{T}
data Type = TVar Sym
          | Arrow Type Type
          | ForAll Sym Type
          deriving (Eq, Read, Show)

-- M[x:=N], where x is free in M
-- substitute free occurrences of 'sym' in 'm' with 'n'
substitution :: LambdaTerm -> Sym -> LambdaTerm -> LambdaTerm
substitution m@(Var var)           sym n | var == sym  = n
                                         | otherwise   = m
substitution   (App p q)           sym n = App (substitution p sym n) (substitution q sym n)
substitution m@(Lam var symType p) sym n | var == sym  = m  -- 'sym' is bound - no substitution
                                         | otherwise   = substitution p sym n
substitution m@(TLam var p)        sym n | var == sym  = m  -- 'sym' is bound - no substitution
                                         | otherwise   = substitution p sym n

betaConversion :: LambdaTerm -> LambdaTerm
betaConversion (App (Lam sym symType term) arg) = substitution term sym arg
betaConversion (App (TLam typeVar term) arg)    = substitution term typeVar arg
betaConversion t = t
