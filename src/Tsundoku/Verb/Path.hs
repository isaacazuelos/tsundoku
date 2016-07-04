-- |
-- Module      : Verb.Path
-- Description : A path verb prints the current pile's path.
-- License     : MIT License
-- Maintainer  : Isaac Azuelos

{-# LANGUAGE OverloadedStrings #-}

module Tsundoku.Verb.Path (verb) where

import qualified Data.Text           as Text
import           Options.Applicative hiding (Success, action, header)
import           System.Directory

import           Tsundoku.IO
import qualified Tsundoku.Pile       as Pile
import           Tsundoku.Verb

-- | The init verb.
verb =
  Verb
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
