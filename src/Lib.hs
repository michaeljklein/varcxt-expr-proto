{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Lib where

import Prelude hiding ((.), id)
import Data.Bifunctor
import Control.Category
import Control.Comonad
import Data.Profunctor
import GHC.Generics
import Control.Arrow

import Data.Void

import Data.Type.Equality -- ((:~:)(..))


-- | @a@ is the `Cxt` of @b@
newtype (:=>) a b = Cxt
  { runCxt :: a -> b
  } deriving (Functor, Applicative, Monad, Category, Profunctor, Arrow)

-- | Variables of type @a@ with ids of type @v@
data V v a = V
  { varId :: v
  , varVal :: a
  } deriving (Eq, Ord, Show, Generic, Generic1, Functor)

instance Foldable (V v) where
  foldr f x = (`f` x) . varVal

instance Traversable (V v) where
  traverse f ~(V var val) = V var <$> f val

instance Bifunctor V where
  bimap f g ~(V var val) = V (f var) (g val)

instance Comonad (V v) where
  extract = varVal

  duplicate vs@(~(V vid _)) = V vid vs


-- | Pattern bindings where variables have id type @v@ and the pattern
-- is bound from matching on @a@'s to a result of type @b@.
newtype Pattern v a b = Pattern
  { runPattern :: forall c d. (a -> Either c b) :=> (V v a -> (V v b :=> PExpr v d) -> Either c (PExpr v d))
  }

-- | Identity pattern
idPatt :: Pattern v a a
idPatt = Pattern $ Cxt $ \f (V vid v) (Cxt cxt) -> cxt . V vid <$> f v

-- | Expressions with variables, lambda/let binding, and literals
--
-- Also with `Pattern`s, but not used much
data PExpr v a where
  PLit :: a -> PExpr v a
  PVar :: (V v a :=> PExpr v a) :~: t -> PExpr v t
  PLambda
    :: (a -> b) :~: t -> v -> Pattern v a a -> PExpr v (V v a :=> PExpr v b) -> PExpr v t
  PLet
    :: v
    -> Pattern v a a
    -> PExpr v (V v a :=> PExpr v b)
    -> PExpr v a
    -> PExpr v b
  PToCxt :: (a :=> b) :~: t -> PExpr v (a -> b) -> PExpr v t

-- | Expressions with variables, lambda/let binding, and literals
data Expr v a where
  Lit :: a -> Expr v a
  Var :: (V v a :=> Expr v a) :~: t -> Expr v t
  Lambda :: (a -> b) :~: t -> v -> Expr v (V v a :=> Expr v b) -> Expr v t
  Let :: v -> Expr v (V v a :=> Expr v b) -> Expr v a -> Expr v b
  ToCxt :: (a :=> b) :~: t -> Expr v (a -> b) -> Expr v t
  App :: Expr v (a -> b) -> Expr v a -> Expr v b

-- | Evaluate a `PExpr`
--
-- @
--  λ> eval (PLambda Refl () idPatt (PVar Refl)) (10 :: Int)
--  10
-- @
--
evalp :: PExpr v a -> a
evalp (PLit x) = x
evalp (PVar refl) = refl `castWith` lmap (PLit . extract) id
evalp (PLambda refl vid (Pattern (Cxt pat)) expr) =
  refl `castWith`
  (\x -> evalp . fromRightVoid $ pat Right (V vid x) (evalp expr))
evalp (PLet vid (Pattern (Cxt pat)) expr xpr) =
  evalp . fromRightVoid $ pat Right (V vid (evalp xpr)) (evalp expr)
evalp (PToCxt refl x) = refl `castWith` Cxt (evalp x)

-- | Evaluate an `Expr`
--
-- @
--  λ> eval (Lambda Refl () (Var Refl)) (10 :: Int)
--  10
-- @
--
eval :: Expr v a -> a
eval (Lit x) = x
eval (Var refl) = refl `castWith` lmap (Lit . extract) id
eval (Lambda refl vid expr) =
  refl `castWith` (\x -> eval $ (runCxt $ eval expr) (V vid x))
eval (Let vid expr xpr) = eval $ (runCxt $ eval expr) (V vid (eval xpr))
eval (ToCxt refl x) = refl `castWith` Cxt (eval x)
eval (App fs xs) = eval fs $ eval xs

-- | Lazy unpacking of `Right`
fromRightVoid :: Either Void a -> a
fromRightVoid ~(Right x) = x

