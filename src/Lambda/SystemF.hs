module Lambda.SystemF where

-- \lambda2, polymorphic typed, second-order typed, second-order polymorphic typed, system F

-- à la Church
type Sym = String
data LambdaTerm = Var Sym                    -- variable
                | App LambdaTerm LambdaTerm  -- application
                | Lam Sym Type LambdaTerm    -- abstraction (\Sym:Type.LambdaTerm)
                | TLam Sym LambdaTerm        -- abstraction by type variable (\Sym.LambdaTerm)
                deriving (Eq, Read)

-- id: (a:*) -> a -> a
-- id = \(a:*).\(x:a).x, id = \(a:*)(x:a).x

instance Show LambdaTerm where
    show (Var sym)               = sym
    show (App (Var s1) (Var s2)) = s1 ++ s2
    show (App t1 t2)             = "(" ++ show t1 ++ ")(" ++ show t2 ++ ")"
    show (Lam sym symType term)  = "\\(" ++ sym ++ ":" ++ show symType ++ ")." ++ show term
    show (TLam sym term)         = "\\(" ++ sym ++ ":*)." ++ show term

-- Types (same for \lambda2-Curry and \lambda2-Church)
-- \mathbb{T} = \mathbb{V} | \mathbb{T} \rightarrow \mathbb{T} | \forall \mathbb{V}.\mathbb{T}
data Type = TVar Sym
          | Arrow Type Type
          | ForAll Sym Type
          deriving (Eq, Read)

instance Show Type where
    show (TVar sym)     = sym
    show (Arrow t1 t2)  = "(" ++ show t1 ++ "->" ++ show t2 ++ ")"
    show (ForAll sym t) = "(" ++ sym ++ ":" ++ show t ++ ")"

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
