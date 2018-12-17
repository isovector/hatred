{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module TH where

import Data.Maybe
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Foldable
import Types


-- parseACommand
--     :: ( Member Hello r
--        , Member World r
--        )
--     => Parsec () String (Document r)
-- parseACommand = do
--   char '\\'
--   cmd <- asum [ string "Hello"
--               , string "World"
--               ]
--   case cmd of
--     "Hello" -> doc <$> commandParser @Hello
--     "World" -> doc <$> commandParser @World


dicts :: Q [Name]
dicts = do
  ClassI _ insts <- reify ''IsCommand
  pure $ mapMaybe isFine insts


getMember :: Name -> Name -> Q Type
getMember r n = [t|
    Member $(pure $ ConT n) $(pure $ VarT r)
  |]


getNameString :: Name -> String
getNameString (Name (OccName str) _) = str


isFine :: InstanceDec -> Maybe Name
isFine (InstanceD _ [] (AppT (ConT _) (ConT t)) _) = Just t
isFine _ = Nothing


buildCmdParser :: Name -> Q Exp
buildCmdParser n = [| doc <$> commandParser @($(pure $ ConT n)) |]


buildCmdSelect :: Name -> Q Exp
buildCmdSelect n = [| string $(liftData $ getNameString n) |]


buildCmdPattern :: Name -> Pat
buildCmdPattern = LitP . StringL . getNameString


buildCmdMatch :: Name -> Q Match
buildCmdMatch n =
  Match
    <$> (pure $ buildCmdPattern n)
    <*> (NormalB <$> buildCmdParser n)
    <*> pure []


makeParser :: [Name] -> Q Exp
makeParser ns = [| do
    char '\\'
    cmd <- asum $(fmap ListE $ traverse buildCmdSelect ns)
    $( CaseE
        <$> [| cmd |]
        <*> traverse buildCmdMatch ns
     )
  |]

makeDocType :: [Name] -> Q Type
makeDocType ns = do
  r <- newName "r"
  z <- traverse (getMember r) ns
  pure $ ForallT [PlainTV r] z $
    ConT ''Parsec
      `AppT` TupleT 0
      `AppT` ConT ''String
      `AppT` (ConT ''Document `AppT` VarT r)

genParser :: String -> [Name] -> Q [Dec]
genParser nameStr ns = do
  let name = Name (OccName nameStr) NameS
  typ  <- makeDocType ns
  body <- makeParser ns
  pure
    [ SigD name typ
    , ValD (VarP name) (NormalB body) []
    ]


