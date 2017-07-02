-- |
-- Module      : Tsundoku.Command.Path
-- Description : A path verb prints the current pile's path.
-- License     : MIT License
-- Maintainer  : Isaac Azuelos

{-# LANGUAGE OverloadedStrings #-}

module Tsundoku.Command.Path (command) where

import           Data.Monoid         ((<>))
import qualified Data.Text           as Text
import           Options.Applicative hiding (Success, action, command, header)
import           System.Directory

import           Tsundoku.Command
import           Tsundoku.IO
import qualified Tsundoku.Pile       as Pile

-- | The path command prints the path of the pile.
command =
  Command
    { name         = "path"
    , header       = "tsundoku path - print the pile's path"
    , description  = "the pile's path"
    , optionParser = optionsParser
    , action       = initAction
    }

-- | Path's options. See the docs for details.
data Options = Options { } deriving (Show, Eq)

-- | An applicative parser for our options.
optionsParser :: Parser Options
optionsParser = pure Options

-- | The entry point into our action, taking the options.
initAction :: Options -> IO Result
initAction Options = pilePath >>= succeedWith . Text.pack
