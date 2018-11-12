{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses,OverloadedStrings #-}
---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2013, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- Test
---------------------------------------------------------


module Main where


import System.IO.Unsafe
import Graphics.PDF
import Data.Maybe(fromJust)
import Control.Monad.IO.Class

import Debug.Trace

vertical = 200.0
margin = 10.0
debugText = "hello world"
debugFontSize = 10
lightBlue= Rgb 0.6 0.6 1.0

testFont="/usr/share/texmf-dist/fonts/type1/public/droid/DroidSans.pfb"
afm="/usr/share/texmf-dist/fonts/afm/public/droid/DroidSans.afm"

data MyParaStyles = DebugStyle AnyFont | Normal
data MyVertStyles = NormalPara


instance ComparableStyle MyParaStyles where
  isSameStyleAs (DebugStyle fa) (DebugStyle fb) = fa == fb
  isSameStyleAs Normal Normal = True
  isSameStyleAs _ _ = False


instance Style MyParaStyles where
    textStyle (DebugStyle f) = TextStyle (PDFFont f debugFontSize) black black FillText 1.0 1.0 1.0 1.0
    textStyle Normal = TextStyle (PDFFont timesRoman debugFontSize) black black FillText 1.0 1.0 1.0 1.0

    sentenceStyle _ = Nothing

    wordStyle _ = Just $ \r m d ->
      case m of
          DrawWord -> d >> setWidth 0.5
          DrawGlue -> d >> setWidth 0.5

    updateStyle a = a

instance ComparableStyle MyVertStyles where
    isSameStyleAs NormalPara NormalPara = True

instance ParagraphStyle MyVertStyles MyParaStyles  where


testAll ::  PDFFont  -> PDF ()
testAll theFont@(PDFFont f s) = do
    page1 <- addPage Nothing
    drawWithPage page1 $ do
      displayFormattedText (Rectangle (10 :+ 0) (100 :+ 380)) NormalPara Normal $ do
        setJustification FullJustification
        paragraph $ do
              txt $ "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor "
              txt $ "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud "
              txt $ "exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute "
              txt $ "irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
              txt $ "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia "
              txt $ "deserunt mollit anim id est laborum."
      displayFormattedText (Rectangle (150 :+ 0) (240 :+ 380)) NormalPara Normal $ do
        setJustification FullJustification
        paragraph $ do
              txt $ "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor "
              txt $ "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud "
              txt $ "exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute "
              txt $ "irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
              txt $ "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia "
              txt $ "deserunt mollit anim id est laborum."


Just timesRoman = unsafePerformIO $ mkStdFont Times_Roman

main :: IO()
main = do
    fontData <- readType1Font testFont afm
    let rect = PDFRect 0 0 600 400
    runPdf "demo.pdf" (standardDocInfo { author= "alpheccar éèçàü", compressed = False}) rect $ do
        testFont <- mkType1Font fontData
        testAll (PDFFont testFont debugFontSize)
        traceM . show $ (spaceGlyph testFont)
        traceM . show $ 1000 * (glyphWidth testFont debugFontSize $ spaceGlyph testFont) / fromIntegral debugFontSize

