-- |
-- Module      : Verb.Template
-- Description : A template verb that does nothing
-- License     : MIT License
-- Maintainer  : Isaac Azuelos

{-# LANGUAGE OverloadedStrings #-}

module Tsundoku.Command.Bump (command) where

import qualified Data.Text           as Text
import           Options.Applicative hiding (Success, action, header, command)
import           System.Directory

import           Tsundoku.IO
import qualified Tsundoku.Pile       as Pile
import           Tsundoku.Command

-- | The init verb.
command =
  Command
    { name         = "bump"
    , header       = "tsundoku bump - bump a book to the top of the list"
    , description  = "Move a book around in the pile of books."
    , optionParser = optionsParser
    , action       = bumpAction
    }

-- | Template's options. See the docs for details.
data Options
  = Options
    { title    :: Text.Text
    , position :: Int }
  deriving (Show, Eq)

optionsParser :: Parser Options
optionsParser = Options
  <$> argument (Text.pack <$> str)
    (metavar "TITLE" <> help "the book's title")
  <*> option auto
    (long "position"
     <> short 'p'
     <> metavar "NUM"
     <> value 1 -- the default is to place books on top of the pile.
     <> help "the position the book should be moved to")

bumpAction :: Options -> IO Result
bumpAction options = do
  path <- pilePath
  pile <- readPile path
  case Pile.move (title options) (pred $ position options) pile of
    Left Pile.NoSuchBookError -> failWith "no book found"
    Left Pile.BookNotUniqueError -> failWith "multiple books found with that title, invalid pile"
    Right newPile -> return $ Success Nothing (Just newPile)
