-- Use -pgmF ⟨cmd⟩ to select the program to use as the preprocessor. When
-- invoked, the ⟨cmd⟩ pre-processor is given at least three arguments on its
-- command-line: the first argument is the name of the original source file,
-- the second is the name of the file holding the input, and the third is the
-- name of the file where ⟨cmd⟩ should write its output to.

module Main where

import Data.Bifunctor
import System.IO
import System.Environment
import Data.List

main :: IO ()
main = do
  [_, fin, fout] <- getArgs
  z <- readFile fin
  writeFile fout . makeFile
                 . fileBits
                 $ lines z


isPassThrough :: String -> Bool
isPassThrough
  = or
  . flip fmap
    [ isPrefixOf "{-#"
    , isPrefixOf "import "
    , isPrefixOf "module "
    , isPrefixOf "-- "
    , (== "")
    ]
  . flip ($)


fileBits :: [String] -> (String, String)
fileBits
  = bimap (unlines . filter noPgmF) unlines
  . span isPassThrough


noPgmF :: String -> Bool
noPgmF = not . isInfixOf "-pgmF"


makeFile :: (String, String) -> String
makeFile (header, content) = unlines
  [ "{-# LANGUAGE FlexibleContexts          #-}"
  , "{-# LANGUAGE NoMonomorphismRestriction #-}"
  , "{-# LANGUAGE QuasiQuotes               #-}"
  , "{-# LANGUAGE EmptyCase                 #-}"
  , "{-# LANGUAGE GADTs                     #-}"
  , header
  , "import Text.Hatred.TH (prose)"
  , ""
  , "contents = [prose|" ++ init content ++ "|]"
  ]


