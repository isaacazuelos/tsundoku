-- |
-- Module      : Verb.Init
-- Description : The verb that sets up piles.
-- License     : MIT License
-- Maintainer  : Isaac Azuelos

{-# LANGUAGE OverloadedStrings #-}

module Tsundoku.Verb.Init (verb) where

import qualified Data.Text           as Text
import           Options.Applicative hiding (Success, action, header)
import           System.Directory

import           Tsundoku.IO
import qualified Tsundoku.Pile       as Pile
import           Tsundoku.Verb

-- | The init verb.
verb =
  Verb
    { name         = "init"
    , header       = "tsundoku init - initialize your pile"
    , description  = "create a new empty pile"
    , optionParser = initOptionParser
    , action       = initAction
    }

-- | Init's options, which are a single reset flag, as stated in the docs.
data InitOptions = InitOptions { reset :: Bool } deriving (Show, Eq)

-- | An applicative parser for our options.
initOptionParser :: Parser InitOptions
initOptionParser = InitOptions <$>
  switch (long "reset" <> help "reset your pile to empty")

-- | The entry point into our action, taking the options.
initAction :: InitOptions -> IO Result
initAction (InitOptions shouldReset) = do
  path <- pilePath
  pileExists <- doesFileExist path
  case (pileExists, shouldReset) of
    (False, False) -> writePile path Pile.empty >> succeedWith "pile created"
    (True,  False) -> failWith "pile already exists"
    (False, True)  -> writePile path Pile.empty >> succeedWith "no pile to reset, pile created"
    (True, True)   -> writePile path Pile.empty >> succeedWith "pile reset"
