module Lambda.P.Term where

import Control.Monad (liftM)
import Data.List (find)

type Sym = String
data LambdaPTerm = Var Sym
                 | Type
                 | Kind
                 | App LambdaPTerm LambdaPTerm
                 | Lam Sym LambdaPTerm LambdaPTerm
                 | Prod Sym LambdaPTerm LambdaPTerm
                 deriving Show

data Context a = Context [(Sym, a)]
               deriving Show

class LambdaCalculus a where
  beta :: a -> a
  checkCtx :: Context a -> Bool
  doType :: a -> Context a -> Maybe a

instance LambdaCalculus LambdaPTerm where
  beta = id

  -- unlike `classic` definition of a context, our context is right-to-left ordered
  checkCtx (Context [])         = True
  checkCtx (Context ((x,t):xs)) = typeOrKind (doType t (Context xs)) == True
                                where typeOrKind (Just Type) = True
                                      typeOrKind (Just Kind) = True
                                      typeOrKind _           = False

  doType (Var x) g@(Context e) | (checkCtx g) = liftM snd (find (\(s, _) -> s == x) e)
                               | otherwise    = Nothing
  doType Type g | (checkCtx g) = Just Kind
                | otherwise    = Nothing
  doType (App m n)    g = Nothing
  doType (Lam x a m)  g = Nothing
  doType (Prod x a b) g = Nothing
  doType _            _ = Nothing

tx = (Var "x")
tg = (Context [("x", (Var "Int")), ("Int", Type)])
