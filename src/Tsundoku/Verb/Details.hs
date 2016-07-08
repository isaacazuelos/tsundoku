-- |
-- Module      : Tsundoku.Verb.Details
-- Description : Show a book's full details
-- License     : MIT License
-- Maintainer  : Isaac Azuelos

{-# LANGUAGE OverloadedStrings #-}

module Tsundoku.Verb.Details (verb) where

import qualified Data.Text           as Text
import           Options.Applicative hiding (Success, action, header)
import           System.Directory

import           Tsundoku.IO
import qualified Tsundoku.Pile       as Pile
import           Tsundoku.Verb
import           Tsundoku.Pretty

-- | The init verb.
verb =
  Verb
    { name         = "details"
    , header       = "tsundoku details - show a book's full details"
    , description  = "print everything known about a book from the pile"
    , optionParser = optionsParser
    , action       = details
    }

-- | Details' options. See the docs for details.
data Options = Options { title :: Text.Text } deriving (Show, Eq)

-- | An parser for our options.
optionsParser :: Parser Options
optionsParser = Options
  <$> argument (Text.pack <$> str)
    (metavar "title"
    <> help "The book's title")

-- | The entry point into our action, taking the options.
details :: Options -> IO Result
details (Options title) = do
  path <- pilePath
  pile <- readPile path
  case Pile.find title pile of
    Left Pile.NoSuchBookError -> failWith "cannot find book to remove"
    Left err -> failWith $ "unexpected error: " <> Text.pack (show err)
    Right book -> succeedWith (pretty True book)
