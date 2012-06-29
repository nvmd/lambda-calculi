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




-- contexts
newtype Env = Env [(Sym, Type)]
              deriving Show

-- empty context
emptyEnv :: Env
emptyEnv = Env []

-- extend the context
extend :: Sym -> Type -> Env -> Env
extend s t (Env r) = Env $ (s, t) : r



betaReduction :: LambdaTerm -> LambdaTerm
betaReduction (App (Lam sym symType term) arg) = term
betaReduction (App (TLam typeVar term) arg) = term
betaReduction t = t
