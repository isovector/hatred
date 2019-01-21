{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Text.Hatred.TH
  ( prose
  ) where

import Data.Bifunctor
import Data.Foldable
import Data.Functor.Identity
import Data.List (nub)
import Data.Maybe
import Data.Semigroup
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Text.Hatred.Types
import Text.Megaparsec
import Text.Megaparsec.Char

notChar :: (MonadParsec e s m, Token s ~ Char) => Char -> m Char
notChar c = satisfy (/= c) <?> "not " ++ [c]
{-# INLINE notChar #-}

anyChar :: ParsecT () String Identity Char
anyChar = satisfy $ const True
{-# INLINE anyChar #-}

-- parseACommand
--     :: ( Member Hello r
--        , Member World r
--        )
--     => Parsec () String (Doc r)
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


genParser :: Name -> [Name] -> Q [Dec]
genParser name ns = do
  body <- makeParser ns
  pure
    [ ValD (VarP name) (NormalB body) []
    ]


stupidParse
    :: Parsec () String [String]
stupidParse = do
  let peekChar = Just <$> (try $ lookAhead anyChar) <|> pure Nothing
  p <- peekChar
  case p of
    Just '\\' -> do
      _ <- char '\\'
      z <- peekChar
      case z of
        Just '\\' -> do
          _ <- char '\\'
          stupidParse
        Just _ -> do
          (:) <$> (many $ noneOf "\\{ ") <*> stupidParse
        Nothing -> error "bad!!"
    Just _ -> many (notChar '\\') *> stupidParse
    Nothing -> pure []


usedCommands :: String -> [String]
usedCommands s =
  either (error "my parser is shit") id $ fmap nub $ parse stupidParse "" s


proseFor :: [Name] -> String -> Q Exp
proseFor ns str = do
  pname <- newName "parser"
  parser <- genParser pname ns

  x <- [|
      either (error "fuck") id
        $ parse
            (parseDocument
                $(pure $ VarE pname))
            ""
            str |]

  pure $ LetE parser x


parseDocument
    :: ( KMember String r
       )
    => (Parsec () String (Doc r))
    -> Parsec () String (Doc r)
parseDocument f = do
  p <- Just <$> (try $ lookAhead anyChar) <|> pure Nothing
  case p of
    Just '\\' -> (<>) <$> f <*> parseDocument f
    Just _ -> (<>) <$> (kdoc <$> many (notChar '\\')) <*> parseDocument f
    Nothing -> pure mempty


prose :: QuasiQuoter
prose = QuasiQuoter
  { quoteExp = \str -> do
      let used = usedCommands str
      cmds <- commands
      let maps = fmap (first getNameString) $ zip cmds cmds
      for_ used $ \use ->
        case any (== use) $ fmap getNameString cmds of
          True  -> pure ()
          False -> error $ "Unknown command " <> use

      let Just names = traverse (flip lookup maps) used
      proseFor names str

  , quoteDec = error ""
  , quoteType = error ""
  , quotePat = error ""
  }


