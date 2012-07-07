module Lambda.SystemF where
import Data.List

-- \lambda2, polymorphic typed, second-order typed, second-order polymorphic typed, system F

i_int = TApp iComb (TVar "Int")
i_int_3 = App (TApp iComb (TVar "Int")) (Var "3")

-- à la Church
type Sym = String
data LambdaTerm = Var Sym                    -- variable
                | App LambdaTerm LambdaTerm  -- application
                | TApp LambdaTerm Type
                | Lam Sym Type LambdaTerm    -- abstraction (\Sym:Type.LambdaTerm)
                | TLam Sym LambdaTerm        -- abstraction by type variable (\Sym.LambdaTerm)
                deriving (Eq, Read)

-- id: (a:*) -> a -> a
-- id = \(a:*).\(x:a).x, id = \(a:*)(x:a).x

instance Show LambdaTerm where
    show (Var sym)               = sym
    show (App (Var s1) (Var s2)) = s1 ++ s2
    show (App t1 t2)             = "(" ++ show t1 ++ ")(" ++ show t2 ++ ")"
    show (TApp t s)              = "(" ++ show t ++ ")(" ++ show s ++ ")"
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

-- contexts
newtype Env = Env [(Sym, Type)]
              deriving Show

-- empty context
emptyEnv :: Env
emptyEnv = Env []

-- extend the context
extend :: Sym -> Type -> Env -> Env
extend s t (Env r) = Env $ (s, t) : r

freeVars :: LambdaTerm -> [Sym]
freeVars (Var v)         = [v]
freeVars (App p q)       = freeVars p `union` freeVars q
freeVars (TApp p s)      = freeVars p
freeVars (Lam v vType t) = freeVars t \\ [v]
freeVars (TLam v t)      = freeVars t \\ [v]

-- M[x:=N], where x is free in M
-- substitute free occurrences of 'sym' in 'm' with 'n'
substitution :: LambdaTerm -> Sym -> LambdaTerm -> LambdaTerm
substitution m@(Var var)           sym n | var == sym  = n
                                         | otherwise   = m
substitution   (App p q)           sym n = App (substitution p sym n) (substitution q sym n)
substitution m@(Lam var varType p) sym n | var == sym  = m  -- 'sym' is bound - no substitution
                                         | var `elem` freeVars n = error (var ++ " \\in FV(" ++ show n ++ ")")
                                         | otherwise   = Lam var varType $ substitution p sym n
substitution m@(TLam var p)        sym n | var == sym  = m  -- 'sym' is bound - no substitution
                                         | otherwise   = TLam var $ substitution p sym n

substitutionWithType :: LambdaTerm -> Sym -> Type -> LambdaTerm
substitutionWithType m@(Lam var varType p) sym n = Lam var (typeSubstitution varType sym n)
                                                           (substitutionWithType p sym n)
substitutionWithType (App p q) sym n = App (substitutionWithType p sym n) (substitutionWithType q sym n)
substitutionWithType (TApp p s) sym n = TApp (substitutionWithType p sym n) s
substitutionWithType m _ _ = m

-- substitute type variable 's' in the type (1st arg.) with type 't'
typeSubstitution :: Type -> Sym -> Type -> Type
typeSubstitution var@(TVar v)  s t | v == s    = t
                                   | otherwise = var
typeSubstitution (Arrow t1 t2) s t = Arrow (typeSubstitution t1 s t) (typeSubstitution t2 s t)
typeSubstitution (ForAll v t1) s t | v == s    = ForAll v t1 -- 's' is bound - no substitution
                                   | otherwise = ForAll v $ typeSubstitution t1 s t

betaConversion :: LambdaTerm -> LambdaTerm
betaConversion (App (Lam sym symType term) arg) = substitution term sym arg
betaConversion (TApp (TLam typeVar term) arg)   = substitutionWithType term typeVar arg
betaConversion t = t

betaReduction :: LambdaTerm -> LambdaTerm
betaReduction redex@(App (Lam x xType m) n) = betaReduction $ betaConversion redex
betaReduction redex@(TApp (TLam x m) n)     = betaReduction $ betaConversion redex
betaReduction m@(App p q) | r == m    = r               -- application can potentially become redex after...
                          | otherwise = betaReduction r -- ...beta-reduction of it's terms
                          where r = App (betaReduction p) (betaReduction q)
betaReduction (Lam v vType p) = Lam v vType $ betaReduction p
betaReduction (TLam v p)      = TLam v $ betaReduction p
betaReduction m = m

-- type checking

isDerivableFromBasis

checkType :: LambdaTerm -> Type -> Bool
checkType m t = True

symIsType :: Sym -> Env -> Bool
symIsType s e = (s, TVar "*") `elem` e

--let e_ff = [("f", ForAll "a" $ Arrow (TVar "a") (TVar "a")), ("b", TVar "*")]

-- check whether the type is derivable from the basis
checkType :: Type -> Env -> Bool
checkType (TVar "*") e = True -- will it be valid if it is present in lambda-term?
checkType (TVar v)   e = isJust vIndex && isJust vType && checkType $ fromJust vType
                         where
                            (_, vType) = find (\(s, _) -> s == v) e -- TODO: move Maybe inside the pair
                            vIndex = elemIndex (v, vType) e
checkType (Arrow t1 t2) e = checkType t1 e && checkType t2 e
checkType (ForAll v t)  e = checkType t $ extend v (TVar "*") e

-- should we work with 'expanded' types?
checkLambdaType :: LambdaTerm -> Env -> Type -> Bool
checkLambdaType (App m n)   e (Arrow t1 t2) = checkLambdaType m e t1 && checkLambdaType n e t2
checkLambdaType (Lam v t m) e (Arrow t1 t2) = t == t1 && checkLambdaType m e t2
checkLambdaType (TLam v m)  e (ForAll v1 t) = v == v1 && checkLambdaType m e t
checkLambdaType (Var v)     e t             = (v, t) `elem` e && checkType t e
checkLambdaType _           _ _             = False

-- standard combinators
iComb :: LambdaTerm
iComb = TLam "a" $ Lam "x" (TVar "a") $ Var "x"
