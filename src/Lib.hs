{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE EmptyCase              #-}
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
{-# OPTIONS_GHC -ddump-splices      #-}

module Lib where

import Types
import Data.Foldable
import Data.Monoid ((<>))
import Data.Coerce
import Data.Functor.Const
import Text.RawString.QQ
import Data.Char (isSpace)
import Language.Haskell.TH.Quote
import Text.Megaparsec
import Text.Megaparsec.Char
import TH



[prose| hello world \World |]




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


-- prose :: Member String r => String -> Document r
-- prose = doc . respectIndentation




parseACommand
    :: ( Member Hello r
       , Member World r
       )
    => Parsec () String (Document r)
parseACommand = do
  char '\\'
  cmd <- asum [ string "Hello"
              , string "World"
              ]
  case cmd of
    "Hello" -> doc <$> commandParser @Hello
    "World" -> doc <$> commandParser @World


parseDocument
    :: ( Member Hello r
       , Member World r
       , Member String r
       )
    => Parsec () String (Document r)
parseDocument = do
  p <- Just <$> (try $ lookAhead anyChar) <|> pure Nothing
  case p of
    Just '\\' -> (<>) <$> parseACommand <*> parseDocument
    Just a -> (<>) <$> (doc <$> many (notChar '\\')) <*> parseDocument
    Nothing -> pure []



doParse
    :: Member String r
    => Parsec () String (Document r)
    -> String
    -> Document r
doParse p s = either (doc . show) id $ parse p "" s


get :: Document '[Hello, World, String] -> String
get = getDoc . interpret (doc . show) . interpret (doc . show)


