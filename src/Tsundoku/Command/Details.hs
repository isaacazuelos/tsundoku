-- |
-- Module      : Tsundoku.Command.Details
-- Description : Show a book's full details
-- License     : MIT License
-- Maintainer  : Isaac Azuelos

{-# LANGUAGE OverloadedStrings #-}

module Tsundoku.Command.Details (command) where

import qualified Data.Text           as Text
import           Options.Applicative hiding (Success, action, command, header)
import           System.Directory

import           Tsundoku.Command
import           Tsundoku.IO
import qualified Tsundoku.Pile       as Pile
import           Tsundoku.Pretty

-- | The details command prints a single book's details.
command =
  Command
    { name         = "details"
    , header       = "tsundoku details - print a book's details"
    , description  = "Print everything known about a book from the pile."
    , optionParser = optionsParser
    , action       = details
    }

-- | Details' options. See the docs for details.
data Options = Options { title :: Text.Text } deriving (Show, Eq)

-- | An parser for our options.
optionsParser :: Parser Options
optionsParser = Options
  <$> argument (Text.pack <$> str)
    (metavar "TITLE"
    <> help "the book's title")

-- | The entry point into our action, taking the options.
details :: Options -> IO Result
details (Options title) = do
  path <- pilePath
  pile <- readPile path
  case Pile.find title pile of
    Left Pile.NoSuchBookError -> failWith "could not find the book to remove"
    Left err -> failWith $ "unexpected error: " <> Text.pack (show err)
    Right book -> succeedWith (pretty True book)
