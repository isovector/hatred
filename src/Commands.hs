{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Commands where

import Text.Hatred.Types
import GHC.Generics
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

data Chapter = Chapter String
  deriving (Generic, Show, IsCommand)

data Section = Section String
  deriving (Generic, Show, IsCommand)

data Emph = Emph String
  deriving (Generic, Show, IsCommand)

data Defn = Defn String
  deriving (Generic, Show, IsCommand)

data Defnn = Defnn String String
  deriving (Generic, Show, IsCommand)

data Ty = Ty String
  deriving (Generic, Show, IsCommand)

data Hs = Hs String
  deriving (Generic, Show, IsCommand)

data Snip = Snip String String
  deriving (Generic, Show, IsCommand)

