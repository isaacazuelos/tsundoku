-- |
-- Module      : Tsundoku.Verb.Delete
-- Description : A Delete verb that does nothing
-- License     : MIT License
-- Maintainer  : Isaac Azuelos

{-# LANGUAGE OverloadedStrings #-}

module Tsundoku.Verb.Remove (verb) where

import qualified Data.Text           as Text
import           Options.Applicative hiding (Success, action, header)
import           System.Directory

import           Tsundoku.IO
import qualified Tsundoku.Pile       as Pile
import           Tsundoku.Verb

-- | The remove verb.
verb =
  Verb
    { name         = "remove"
    , header       = "tsundoku remove - remove a book from the pile"
    , description  = "remove a book from your pile"
    , optionParser = optionsParser
    , action       = initAction
    }

-- | Delete's options. See the docs for details.
data Options = Options { title :: Text.Text } deriving (Show, Eq)

-- | An applicative parser for our options.
optionsParser :: Parser Options
optionsParser = Options
  <$> argument (Text.pack <$> str)
    (metavar "title"
    <> help "The book's title")

-- | The entry point into our action, taking the options.
initAction :: Options -> IO Result
initAction options = do
  path <- pilePath
  pile <- readPile path
  case Pile.delete (title options) pile of
    Left Pile.NoSuchBookError -> failWith "cannot find book to remove"
    Left err -> failWith $ "unexpected error: " <> Text.pack (show err)
    Right pile' -> return Success { message = Nothing, updated = Just pile' }
