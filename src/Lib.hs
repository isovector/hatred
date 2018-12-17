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

module Lib where

import Data.Monoid ((<>))
import Data.Coerce
import Data.Functor.Const
import Text.RawString.QQ
import Data.Char (isSpace)
import Language.Haskell.TH.Quote

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

doc :: Member a r => a -> Document r
doc = pure . inject


data World = World
  deriving Show


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


run :: Sum '[r] -> r
run (One a) = a

getDoc :: Monoid r => Document '[r] -> r
getDoc = foldMap run


respectIndentation :: String -> String
respectIndentation z =
  case dropWhile (== "") $ lines z of
    [] -> []
    (a : as) ->
      let (spaces, a') = span isSpace a
       in unlines $ a' : fmap (drop $ length spaces) as


prose :: Member String r => String -> Document r
prose = doc . respectIndentation


foo :: Member String r => Document r
foo = prose [r|
  hello \world
  |]


worlded :: String
worlded = getDoc
        . interpret (doc . show @World)
        . relay parseWorld
        $ foo
  where
    parseWorld ('\\' : 'w' : 'o' : 'r' : 'l' : 'd' : xs) =
      doc World <> parseWorld xs
    parseWorld (a : as) = doc [a] <> parseWorld as
    parseWorld [] = []

