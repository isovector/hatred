{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Text.Hatred.Types2 where

-- import Data.Functor.Foldable (Doc (..))
import Data.Functor.Classes
import Data.Functor.Compose
import Data.Bifunctor
import Data.Coerce
import Data.Functor.Const


data OneOf fs a where
  One :: f a -> OneOf '[f] a
  Cons :: Either (f a) (OneOf fs a) -> OneOf (f ': fs) a

newtype Doc fs = Doc [OneOf fs (Doc fs)]
  deriving (Semigroup, Monoid) via [OneOf fs (Doc fs)]

deriving via [OneOf fs (Doc fs)]
  instance (forall x. Show x => Show (OneOf fs x)) => Show (Doc fs)


instance {-# OVERLAPPING #-} Functor f => Functor (OneOf '[f]) where
  fmap f (One a) = One $ fmap f a

instance (Functor f, Functor (OneOf fs)) => Functor (OneOf (f ': fs)) where
  fmap f (Cons a) = Cons $ bimap (fmap f) (fmap f) a


instance {-# OVERLAPPING #-} Show (f x) => Show (OneOf '[f] x) where
  show (One a) = show a

instance (Show (f x), Show (OneOf fs x)) => Show (OneOf (f ': fs) x) where
  show (Cons (Left a))  = show a
  show (Cons (Right a)) = show a


class Member f fs where
  inject :: f a -> OneOf fs a
  cast :: OneOf fs a -> Maybe (f a)

instance {-# OVERLAPPING #-} Member f '[f] where
  inject = One
  cast = Just . run

instance Member f (f ': fs) where
  inject = Cons . Left
  cast (Cons (Left a))  = Just a
  cast (Cons (Right _)) = Nothing

instance Member f fs => Member f (g ': fs) where
  inject = Cons . Right . inject
  cast (Cons (Left _))  = Nothing
  cast (Cons (Right a)) = cast a


class Members as fs
instance (Member a fs, Members as fs) => Members (a ': as) fs
instance Members '[] fs


unDoc :: Doc fs -> [OneOf fs (Doc fs)]
unDoc = coerce


doc :: Member f fs => f (Doc fs) -> Doc fs
doc = Doc . pure . inject

kdoc :: Member (Const a) fs => a -> Doc fs
kdoc = doc . Const


run :: OneOf '[f] a -> f a
run (One a) = a

