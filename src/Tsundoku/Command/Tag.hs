-- |
-- Module      : Tsundoku.Verb.Tag
-- Description : The verb for managing tags.
-- License     : MIT License
-- Maintainer  : Isaac Azuelos

{-# LANGUAGE OverloadedStrings #-}

module Tsundoku.Command.Tag (command) where

import qualified Data.Text           as Text
import           Options.Applicative hiding (Success, action, command, header)
import           System.Directory

import qualified Tsundoku.Book       as Book
import           Tsundoku.IO
import qualified Tsundoku.Pile       as Pile
import           Tsundoku.Command

-- | The tag command lets you modify tags.
command =
  Command
    { name         = "tag"
    , header       = "tsundoku tag - manage tags"
    , description  = "Manage a book's tags."
    , optionParser = optionsParser
    , action       = initAction
    }

-- | Template's options. See the docs for details.
data Options
  = Options
    { title  :: Text.Text
    , remove :: Bool
    , tag    :: Text.Text
    }
  deriving (Show, Eq)

-- | An applicative parser for our options.
optionsParser :: Parser Options
optionsParser = Options
  <$> argument (Text.pack <$> str)
    (metavar "TITLE"
    <> help "The book's title")
  <*> switch
    (long "remove"
    <> short 'r'
    <> help "remove the tag")
  <*> argument (Text.pack <$> str)
    (metavar "TAG"
    <> help "the tag to add or remove")

-- | The entry point into our action, taking the options.
initAction :: Options -> IO Result
initAction (Options title remove tag) = do
  pile <- pilePath >>= readPile
  case Pile.find title pile of
    Left Pile.NoSuchBookError -> failWith "no book found with that title"
    Left err -> failWith . Text.pack $ "unexpected error: " ++ show err
    Right book -> do
      let book' = (if remove then removeTag else addTag) book tag
      case Pile.delete title pile >>= Pile.add book' of
        Left err -> failWith . Text.pack $ "unexpected error: " ++ show err
        Right pile' -> return $ Success Nothing (Just pile')

removeTag :: Book.Book -> Book.Tag -> Book.Book
removeTag book tag = book { Book.tags = filter (/= tag) (Book.tags book)}

addTag :: Book.Book -> Book.Tag -> Book.Book
addTag book tag = if tag `elem` Book.tags book
  then book
  else book { Book.tags = tag : Book.tags book }
