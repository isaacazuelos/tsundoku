-- |
-- Module      : Verb.Template
-- Description : A template verb that does nothing
-- License     : MIT License
-- Maintainer  : Isaac Azuelos

{-# LANGUAGE OverloadedStrings #-}

module Tsundoku.Verb.Template (verb) where

import qualified Data.Text           as Text
import           Options.Applicative hiding (Success, action, header)
import           System.Directory

import           Tsundoku.IO
import qualified Tsundoku.Pile       as Pile
import           Tsundoku.Verb

-- | The init verb.
verb =
  Verb
    { name         = "template"
    , header       = "tsundoku init - does nothing"
    , description  = "do nothing"
    , optionParser = optionsParser
    , action       = initAction
    }

-- | Template's options. See the docs for details.
data Options
  = Options
    { }
  deriving (Show, Eq)

-- | An applicative parser for our options.
optionsParser :: Parser Options
optionsParser = pure Options

-- | The entry point into our action, taking the options.
initAction :: Options -> IO Result
initAction _ = return $ Success Nothing Nothing
