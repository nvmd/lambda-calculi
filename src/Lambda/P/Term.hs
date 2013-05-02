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

isTypeOrKind (Just Type) = True
isTypeOrKind (Just Kind) = True
isTypeOrKind _           = False

isType (Just Type) = True
isType _           = False

isTypeOrKind2 j@(Just Type) = j
isTypeOrKind2 j@(Just Kind) = j
isTypeOrKind2 _             = Nothing

isType2 j@(Just Type) = j
isType2 _             = Nothing

data Context a = Context [(Sym, a)]
               deriving Show

-- empty context
emptyCtx :: LambdaCalculus a => Context a
emptyCtx = Context []

-- extend the context
extendCtx :: LambdaCalculus a => Sym -> a -> Context a -> Context a
extendCtx s t (Context e) = Context $ (s, t) : e

class LambdaCalculus a where
  beta :: a -> a
  checkCtx :: Context a -> Bool
  doType :: a -> Context a -> Maybe a

instance LambdaCalculus LambdaPTerm where
  beta = id

  -- unlike `classic` definition of a context, our context is right-to-left ordered
  checkCtx (Context [])         = True
  checkCtx (Context ((x,t):xs)) = isTypeOrKind (doType t (Context xs)) == True

  doType (Var x) g@(Context e) | (checkCtx g) = liftM snd (find (\(s, _) -> s == x) e)
                               | otherwise    = Nothing
  doType Type g | (checkCtx g) = Just Kind
                | otherwise    = Nothing
  doType (App m n)    g = Nothing
  doType (Lam x a m)  g = isType2 $ doType a g >>  doType m (extendCtx x a g)
                                               >>= return . (\b -> Prod x a b)
  doType (Prod x a b) g = isType2 $ doType a g >> isTypeOrKind2 bType
                        where bType = doType b (extendCtx x a g)
  doType _            _ = Nothing



tx = (Var "x")
tg = (Context [("x", (Var "Int")), ("Int", Type)])
