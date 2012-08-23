module Lambda.SystemF.TypeChecking where

import Data.Maybe
import Data.List
import Lambda.SystemF.Term

-- type checking

symIsType :: Sym -> Env -> Bool
symIsType s (Env e) = (s, TVar "*") `elem` e

--let e_ff = [("f", ForAll "a" $ Arrow (TVar "a") (TVar "a")), ("b", TVar "*")]

-- check whether the type (Type) is derivable from the basis (Env)
checkType :: Type -> Env -> Bool
checkType (TVar "*")    _  = True -- will it be valid if it is present in lambda-term?
checkType (TVar v) (Env e) = isJust vIndex && isJust vType && checkType (fromJust vType) (Env e)
                           where
                            (_, vType) = maybePairToPairMaybe $ find (\(s, _) -> s == v) e
                            vIndex     = elemIndex (v, fromJust vType) e
checkType (Arrow t1 t2) e = checkType t1 e && checkType t2 e
checkType (ForAll v t)  e = checkType t $ extend v (TVar "*") $ extend v (TVar "*") e

expandType :: Type -> Env -> Type
expandType (TVar v) (Env e) | isJust vType && fromJust vType /= (TVar "*") = fromJust vType
                            | isJust vType = (TVar v)
                            where
                                (_, vType) = maybePairToPairMaybe $ find (\(s, _) -> s == v) e
expandType (Arrow t1 t2) e = Arrow (expandType t1 e) (expandType t2 e)
expandType (ForAll v t)  e = ForAll v $ expandType t $ extend v (TVar "*") e

maybePairToPairMaybe :: Maybe (a, b) -> (Maybe a, Maybe b)
maybePairToPairMaybe (Just (s, t)) = (Just s, Just t)
maybePairToPairMaybe _             = (Nothing, Nothing)

-- should we work with 'expanded' types?
checkTermType :: LambdaTerm -> Env -> Type -> Bool
checkTermType (App m n)   e (Arrow t1 t2) = checkType t1 e && checkTermType m e t1
                                              && checkType t2 e && checkTermType n e t2
checkTermType (TApp (TLam v m) n) e t     = checkType t e
                                              && checkTermType (substitutionWithType m v n)
                                                (extend v n e) (typeSubstitution t v n)
checkTermType (Lam v t m) e (Arrow t1 t2) = t == t1 && checkType t e
                                              && checkTermType m (extend v t e) t2
checkTermType (TLam v m)  e (ForAll v1 t) = v == v1
                                              && checkTermType m (extend v (TVar "*") e) t
checkTermType (Var v) (Env e) t = (v, t) `elem` e && checkType t (Env e)
checkTermType _       _       _ = False
