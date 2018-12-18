{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}

module Types where

import Data.Foldable
import Data.Monoid ((<>))
import Data.Coerce
import Data.Functor.Const
import Text.RawString.QQ
import Data.Char (isSpace)
import Language.Haskell.TH.Quote
import Text.Megaparsec
import Text.Megaparsec.Char
import Language.Haskell.TH.Syntax
import GHC.Generics

data Sum ts where
  One  :: a -> Sum '[a]
  Cons :: Either a (Sum as) -> Sum (a ': as)

instance (Lift (Sum ts), Lift t) => Lift (Sum (t ': ts)) where
  lift (Cons a) = [| Cons $(lift a) |]

instance {-# OVERLAPPING #-} (Lift t) => Lift (Sum '[t]) where
  lift (One a) = [| One $(lift a) |]


class Member a as where
  inject :: a -> Sum as
  cast :: Sum as -> Maybe a

instance {-# OVERLAPPING #-} Member a '[a] where
  inject = One
  cast = Just . run

instance Member a (a ': as) where
  inject = Cons . Left
  cast (Cons (Left a)) = Just a
  cast _ = Nothing

instance {-# OVERLAPPABLE #-} Member a as => Member a (b ': as) where
  inject = Cons . Right . inject
  cast (Cons (Right a)) = cast a
  cast _ = Nothing

type Document r = [Sum r]

decompose :: Sum (a ': as) -> Either a (Sum as)
decompose (Cons z) = z


weaken :: Sum as -> Sum (a ': as)
weaken = Cons . Right


class IsCommand a where
  commandParser :: Parsec () String a
  default commandParser
      :: (Generic a, GIsCommand (Rep a))
      => Parsec () String a
  commandParser = to <$> gcommandParser


class GIsCommand a where
  gcommandParser :: Parsec () String (a x)

instance GIsCommand U1 where
  gcommandParser = pure U1

instance IsCommand a => GIsCommand (K1 _1 a) where
  gcommandParser = K1 <$> do
    char '{'
    z <- commandParser
    char '}'
    pure z

instance (GIsCommand f, GIsCommand g) => GIsCommand (f :*: g) where
  gcommandParser = (:*:) <$> gcommandParser <*> gcommandParser

instance (GIsCommand f) => GIsCommand (M1 _1 _2 f) where
  gcommandParser = M1 <$> gcommandParser



run :: Sum '[r] -> r
run (One a) = a


doc :: Member a r => a -> Document r
doc = pure . inject

data Hello = Hello String
  deriving Show

instance IsCommand Hello where
  commandParser = do
    char '{'
    z <- many . satisfy $ \a -> a /= '{' && a /= '}'
    char '}'
    Hello <$> pure z

data World = World
  deriving (Generic, Show, IsCommand)

