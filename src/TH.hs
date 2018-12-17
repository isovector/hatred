{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module TH
  ( prose
  ) where

import Data.Bifunctor
import Data.Traversable (for)
import Data.Monoid
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


commands :: Q [Name]
commands = do
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

makeParserType :: [Name] -> Q Type
makeParserType ns = do
  r <- newName "r"
  z <- traverse (getMember r) $ ''String : ns
  pure $ ForallT [PlainTV r] z $
    ConT ''Parsec
      `AppT` TupleT 0
      `AppT` ConT ''String
      `AppT` (ConT ''Document `AppT` VarT r)

makeDocType :: [Name] -> Q (Type, Name)
makeDocType ns = do
  r <- newName "r"
  z <- traverse (getMember r) $ ''String : ns
  pure
    ( ForallT [PlainTV r] z $ ConT ''Document `AppT` VarT r
    , r
    )


genParser :: Name -> [Name] -> Q [Dec]
genParser name ns = do
  typ  <- makeParserType ns
  body <- makeParser ns
  pure
    [ SigD name typ
    , ValD (VarP name) (NormalB body) []
    ]


stupidParse
    :: Parsec () String [String]
stupidParse = do
  let peekChar = Just <$> (try $ lookAhead anyChar) <|> pure Nothing
  p <- peekChar
  case p of
    Just '\\' -> do
      char '\\'
      z <- peekChar
      case z of
        Just '\\' -> do
          char '\\'
          stupidParse
        Just _ -> do
          (:) <$> (many $ noneOf "\\{ ") <*> stupidParse
        Nothing -> error "bad!!"
    Just a -> many (notChar '\\') *> stupidParse
    Nothing -> pure []


usedCommands :: String -> [String]
usedCommands s =
  either (error "my parser is shit") id $ parse stupidParse "" s


makeTuple :: [Type] -> Type
makeTuple t = foldl (AppT) (TupleT $ length t) t

proseFor :: [Name] -> String -> Q [Dec]
proseFor ns str = do
  name  <- newName "prose"
  pname <- newName "parser"


  parser <- genParser pname ns
  (doctype, r) <- makeDocType ns
  z <- traverse (getMember r) $ ''String : ns

  x <- [|
      either (error "fuck") id
        $ parse
            (parseDocument
                @($(pure $ makeTuple z))
                $(pure $ VarE pname))
            ""
            str |]


  pure $
    [ SigD name doctype
    , ValD (VarP name) (NormalB x) parser
    ]


parseDocument
    :: forall c r
     . ( c
       , Member String r
       )
    => (c => Parsec () String (Document r))
    -> Parsec () String (Document r)
parseDocument f = do
  p <- Just <$> (try $ lookAhead anyChar) <|> pure Nothing
  case p of
    Just '\\' -> (<>) <$> f <*> parseDocument @c f
    Just a -> (<>) <$> (doc <$> many (notChar '\\')) <*> parseDocument @c f
    Nothing -> pure []


prose :: QuasiQuoter
prose = QuasiQuoter
  { quoteDec = \str -> do
      let used = usedCommands str
      cmds <- commands
      let maps = fmap (first getNameString) $ zip cmds cmds
      for_ used $ \use ->
        case any (== use) $ fmap getNameString cmds of
          True  -> pure ()
          False -> error $ "Unknown command " <> use

      let Just names = traverse (flip lookup maps) used
      proseFor names str

  , quoteExp = error ""
  , quoteType = error ""
  , quotePat = error ""
  }


