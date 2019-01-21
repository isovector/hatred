{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE EmptyCase                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeOperators             #-}
{-# OPTIONS_GHC -ddump-splices         #-}

module Text.Hatred.Lib where

import Control.Monad
import Control.Applicative
import Data.Char (isSpace)
import Data.Coerce
import Data.Foldable
import Data.Functor.Const
import Data.Monoid ((<>))
import Language.Haskell.TH.Quote
import Text.Hatred.TH
import Text.Hatred.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.RawString.QQ


------------------------------------------------------------------------------
-- | Fold a document into a single value.
cata
    :: ( Functor (OneOf fs)
       , Monoid a
       )
    => (OneOf fs a -> a)
    -> Doc fs
    -> a
cata f (Doc z) = foldMap (f . fmap (cata f)) z


------------------------------------------------------------------------------
-- | Fold a document monadically.
cataM
    :: ( Functor (OneOf fs)
       , Monad m
       , Monoid a
       , Foldable (OneOf fs)
       )
    => (OneOf fs a -> m a)
    -> Doc fs
    -> m a
cataM f = foldr (liftA2 (<>) . cataM f . Doc . pure)
                (pure mempty)
        . unDoc


------------------------------------------------------------------------------
-- | Replace a capability with a subdocument.
cata1
    :: Functor (OneOf fs)
    => (f (Doc (f ': fs)) -> Doc fs)
    -> Doc (f ': fs)
    -> Doc fs
cata1 f (Doc z) = Doc $ z >>=
  either (pure . fmap (cata1 f))
         (unDoc . f)
    . decompose


------------------------------------------------------------------------------
-- | Replace a constant term with a subdocument.
interpret
    :: Functor (OneOf fs)
    => (k -> Doc fs)
    -> Doc (Const k ': fs)
    -> Doc fs
interpret f = cata1 (f . getConst)


------------------------------------------------------------------------------
-- | Create structure around a capability.
relay
    :: forall f fs
     . Member f fs
    => (f (Doc fs) -> Doc fs)
    -> Doc fs
    -> Doc fs
relay f (Doc z) = Doc $ z >>= \x ->
  case cast @f x of
    Just y  -> unDoc $ f y
    Nothing -> pure x


respectIndentation :: String -> String
respectIndentation z =
  case dropWhile (== "") $ lines z of
    [] -> []
    (a : as) ->
      let (spaces, a') = span isSpace a
       in unlines $ a' : fmap (drop $ length spaces) as



-- get :: Doc '[Hello, World, String] -> String
-- get = getDoc . interpret (doc . show) . interpret (doc . show)

