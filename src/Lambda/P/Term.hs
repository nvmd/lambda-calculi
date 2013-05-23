module Lambda.P.Term where

import Control.Monad --(mfilter, MonadPlus)
import Data.Maybe (fromMaybe, isJust)

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

isType2 = mfilter (\x -> case x of
                           Type -> True
                           _    -> False)

isTypeOrKind2 = mfilter (\x -> case x of
                                 Type -> True
                                 Kind -> True
                                 _    -> False)

isProd2 = mfilter (\x -> case x of
                           Prod x a b -> True
                           _          -> False)

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

alphaEqStub :: LambdaPTerm -> LambdaPTerm -> Bool
alphaEqStub m n = alphaEq [] m [] n

alphaEq :: [Sym] -> LambdaPTerm -> [Sym] -> LambdaPTerm -> Bool
-- alphaEq (typeInCtx ctx1 x) (typeInCtx ctx2 y)
alphaEq bound1 (Var x)      bound2 (Var y)      = x == y || (   x `elem` bound1
                                                             && y `elem` bound2)
alphaEq bound1 Type         bound2 Type         = True
alphaEq bound1 Kind         bound2 Kind         = True
alphaEq bound1 (App m n)    bound2 (App p q)    =  alphaEq bound1 m bound2 p
                                                && alphaEq bound1 n bound2 q
alphaEq bound1 (Lam x a m)  bound2 (Lam y b n)  = undefined &&
                                                   alphaEq    bound1 a     bound2 b
                                                && alphaEq (x:bound1) m (x:bound2) n
alphaEq bound1 (Prod x a b) bound2 (Prod y c d) = undefined
alphaEq _      _            _      _            = False

-- ->_\beta
betaConv :: LambdaPTerm -> Context LambdaPTerm -> LambdaPTerm
betaConv p@(App (Lam x a m) n)  ctx = fromMaybe p $
                                      doType n ctx >>= \t ->
                                      betaEqM t a  >>
                                      return (substitution m x n)
betaConv p@(App (Prod x a b) n) ctx = fromMaybe p $
                                      doType n ctx >>= \t ->
                                      betaEqM t a  >>
                                      return (substitution b x n)
betaConv p                      ctx = p

-- ->>_\beta
--betaRed :: LambdaCalculus a => a -> a
betaRed t = undefined

-- =_\beta
--betaEq :: LambdaCalculus a => a -> a -> Bool
betaEq m n = isJust $ betaEqM m n

--betaEqM :: LambdaCalculus a => a -> a -> Maybe a
betaEqM m n = if alphaEqStub mRed nRed then return mRed else mzero
            where mRed = betaRed m
                  nRed = betaRed n

-- M[x := N], where M, N are terms, x is a variable
substitution :: LambdaPTerm -> Sym -> LambdaPTerm -> LambdaPTerm
substitution m@(Var v)      x n | v == x    = n
                                | otherwise = m
substitution m@(Type)       x n = m
substitution m@(Kind)       x n = m
substitution   (App p q)    x n = App (substitution p x n) (substitution q x n)
substitution   (Lam y b p)  x n = Lam y (substitution b x n) (if y == x then p else substitution p x n)
                                -- 'y' is bound - no substitution in P, only in B
                                -- because '\y:B.P' bounds 'y' inside P, not B
                                -- | y `elem` freeVars n = error (y ++ " \\in FV(" ++ show n ++ ")")
substitution   (Prod y b c) x n = Prod y (substitution b x n) (if y == x then c else substitution c x n)

instance LambdaCalculus LambdaPTerm where
  beta = id

  -- unlike `classic` definition of a context, our context is right-to-left ordered
  checkCtx (Context [])         = True
  checkCtx (Context ((x,t):xs)) = isTypeOrKind (doType t (Context xs)) == True

-- typing due to geuvers
  doType (Var x) g@(Context e) | (checkCtx g) = lookup x e
                               | otherwise    = Nothing
  doType Type g | (checkCtx g) = return Kind
                | otherwise    = mzero
  doType (App m n)    g = isProd2 $ doType m g >>= \p ->
                                    doType n g >>= \d ->
                                    case p of
                                      Prod x a b -> mfilter (\_ -> a `betaEq` d)
                                                            (return $ substitution b x n)
  doType (Lam x a m)  g = isType2 $ doType a g >>  doType m (extendCtx x a g)
                                               >>= return . (\b -> Prod x a b)
  doType (Prod x a b) g = isType2 $ doType a g >> isTypeOrKind2 bType
                        where bType = doType b (extendCtx x a g)
  doType _            _ = Nothing



tx = (Var "x")
tg = (Context [("x", (Var "Int")), ("Int", Type)])
