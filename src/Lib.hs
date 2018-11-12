{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections      #-}

module Lib where

import Data.Maybe (fromJust)
import Data.Bool
import Control.Applicative
import Control.Monad
import Data.Data
import Data.Foldable
import Data.Monoid
import GHC.Generics


data Block
  = Heading Int Inline
  | Plain [Inline]
  | Paragraph [Inline]

data Inline
  = Str String
  | HardBreak
  | Space

-- data Layout
--   = Box Point Size
--   | Anchor Side Dimension

-- data Flow
--    = Flow [Layout]

data Side
  = TopSide
  | LeftSide
  | RightSide
  | BottomSide

data Document

data Physical

-- append semantics?
instance Monoid Document where
  mempty  = undefined
  mappend = undefined

type Point = (Int, Int)
type Size = (Dimension, Dimension)
type Dimension = Int

-- flow :: Block -> Flow -> Document
-- flow = undefined

overlay :: Document -> Document -> Document
overlay = undefined

-- ???
paginate :: Document -> Size -> Physical
paginate = undefined


data Glue x = Glue
  { glueShrink  :: Float
  , glueSize    :: Float
  , glueStretch :: Float
  }
  deriving (Generic, Typeable, Data, Show)

instance Monoid (Glue x) where
  mempty = Glue 0 0 0
  mappend (Glue a1 a2 a3)
          (Glue b1 b2 b3) = Glue (a1 + b1) (a2 + b2) (a3 + b3)

data Box = Box
  { boxWidth  :: Float
  , boxHeight :: Float
  }
  deriving (Generic, Typeable, Data, Show)

data Horizontal

type HBox = Either Box (Glue Horizontal)

bakeH :: Traversable t => t HBox -> [HBox]
bakeH = bake . toList
  where
    bake [] = []
    bake (Left b1 : Left b2 : xs) =
      bake $ (Left $ Box (boxWidth b1 + boxWidth b2) $ max (boxHeight b1) (boxHeight b2)) : xs
    bake (Right g1 : Right g2 : xs) =
      bake $ (Right $ g1 <> g2) : xs
    bake (x : xs) = x : bake xs

getHSize :: HBox -> Glue Horizontal
getHSize (Left (Box w _)) = Glue 0 w 0
getHSize (Right g)        = g

getSize :: Foldable t => t HBox -> Glue Horizontal
getSize = foldMap getHSize

data Region = Region
  { regWidth  :: Float
  , regHeight :: Float
  }
  deriving (Generic, Typeable, Data)

makeBox :: Char -> HBox
makeBox ' ' = Right $ Glue 0.5 1 0.5
makeBox c   = Left  $ Box 1 2

boxItUp :: String -> [HBox]
boxItUp = bakeH . fmap makeBox

minSize :: Glue x -> Float
minSize = liftA2 (-) glueSize glueShrink

maxSize :: Glue x -> Float
maxSize = liftA2 (+) glueSize glueStretch

data Interval a = Interval
  { iMin :: a
  , iMax :: a
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

stretchability :: [Glue x] -> Interval Float
stretchability g =
  let Glue l v h = mconcat g
   in Interval (v - l) (v + h)

type Badness = Float

-- stretch' :: Float -> (Glue x -> Float) -> Glue x -> Maybe (Glue x)
-- stretch' sr f g@(Glue l w h) = do
--   guard $ sr >= -1
--   guard $ sr <= 1
--   let s = f g * sr
--   pure $ Glue (l + s) (w + s) (h - s)

stretch :: Float -> Glue x -> Maybe (Glue x)
stretch sr (Glue l w h) = do
  guard $ sr >= -1
  guard $ sr <= 1
  let s = bool l h (sr > 0) * sr
  pure $ Glue (l + s) (w + s) (h - s)

setHGlue
    :: Float  -- ^ desired width
    -> [HBox]
    -> Maybe ([HBox], Badness)
setHGlue desired hbox = do
  let together = getSize hbox
      size     = glueSize together
      delta    = desired - size
  guard $ minSize together <= desired
       && desired <= maxSize together
  pure $ (, delta ^ 2) $ case signum delta of
    0  -> hbox
    1  -> fmap (fmap $ fromJust . stretch (delta / glueStretch together)) hbox
    -1 -> fmap (fmap $ fromJust . stretch (delta / glueShrink together)) hbox


