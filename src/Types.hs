{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
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

data Sum ts where
  One  :: a -> Sum '[a]
  Cons :: Either a (Sum as) -> Sum (a ': as)


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
  deriving Show

instance IsCommand World where
  commandParser = pure World

