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
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Text.Hatred.Types3 where

import Data.Functor.Classes
import Data.Functor.Compose
import Data.Bifunctor
import Data.Coerce
import Data.Functor.Const

import Text.Hatred.Types2 (OneOf (..))


data Block bs ss b
    = Block (OneOf bs b)
    | Span  (Doc ss)

newtype DoubleFix bs ss = DoubleFix (Block bs ss (DoubleFix bs ss))

newtype Doc fs = Doc [OneOf fs (Doc fs)]
  deriving (Semigroup, Monoid) via [OneOf fs (Doc fs)]

deriving via [OneOf fs (Doc fs)]
  instance (forall x. Show x => Show (OneOf fs x)) => Show (Doc fs)



