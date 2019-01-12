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

module Lib where

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




interpret
    :: (r -> Document rs)
    -> Document (r ': rs)
    -> Document rs
interpret f = (either f pure . decompose =<<)


relay
    :: Member r rs => (r -> Document rs)
    -> Document rs
    -> Document rs
relay f = (maybe [] f . cast =<<)


getDoc :: Monoid r => Document '[r] -> r
getDoc = foldMap run


respectIndentation :: String -> String
respectIndentation z =
  case dropWhile (== "") $ lines z of
    [] -> []
    (a : as) ->
      let (spaces, a') = span isSpace a
       in unlines $ a' : fmap (drop $ length spaces) as


doParse
    :: Member String r
    => Parsec () String (Document r)
    -> String
    -> Document r
doParse p s = either (doc . show) id $ parse p "" s


-- get :: Document '[Hello, World, String] -> String
-- get = getDoc . interpret (doc . show) . interpret (doc . show)

