module Lambda.SimplyTyped where
import Lambda.Pure
import Data.List

-- t = (a -> b) -> c
t = Arrow (Arrow (TVar "a") (TVar "b")) (TVar "c")
-- t = ((TVar "a") :-> (TVar "b")) :-> (TVar "c")

-- Types
data Type = TVar Sym
          | Arrow Type Type
          deriving (Eq, Read, Show)

-- all elements of the type (in this type system)
freeTypeVars :: Type -> [Sym]
freeTypeVars (TVar v) = [v]
freeTypeVars (Arrow t1 t2) = freeTypeVars t1 `union` freeTypeVars t2

-- contexts
newtype Env = Env [(Sym, Type)]
              deriving Show

-- empty context
emptyEnv :: Env
emptyEnv = Env []

-- extend the context
extend :: Sym -> Type -> Env -> Env
extend s t (Env r) = Env $ (s, t) : r

-- Type substitution --- S: \mathbb{T} \rightarrow \mathbb{T}
-- S(\sigma \rightarrow \tau) \equiv S(\sigma) \rightarrow S(\tau)
type TypeSubstitution = [(Type, Type)]

-- stub
applySubstitution :: TypeSubstitution -> Type -> Type
applySubstitution s t = t

-- stub
substitutionSuperposition :: TypeSubstitution -> TypeSubstitution
                                              -> TypeSubstitution
substitutionSuperposition s1 s2 = s1

-- unification algorithm
algU :: Type -> Type -> TypeSubstitution
algU (TVar t1) (TVar t2) | (TVar t1) == (TVar t2) = []
algU (TVar t1) t2 | t1 `elem` freeTypeVars t2 = error "Can't unify"
                  | otherwise                 = [(TVar t1, t2)]
algU (Arrow t1 t2) (TVar t3)     = algU (TVar t3) (Arrow t1 t2)
algU (Arrow s1 s2) (Arrow t1 t2) = substitutionSuperposition
                                   (algU (applySubstitution (algU s2 t2) s1)
                                         (applySubstitution (algU s2 t2) t1))
                                   (algU s2 t2)
--algE :: Env -> Expr -> Type ->
--algPP
--algPT
