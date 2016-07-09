-- |
-- Module      : Tsundoku.Command.Init
-- Description : The command to create a new pile.
-- License     : MIT License
-- Maintainer  : Isaac Azuelos

{-# LANGUAGE OverloadedStrings #-}

module Tsundoku.Command.Init (command) where

import qualified Data.Text           as Text
import           Options.Applicative hiding (Success, action, command, header)
import           System.Directory

import           Tsundoku.Command
import           Tsundoku.IO
import qualified Tsundoku.Pile       as Pile

-- | The init command sets up piles.
command =
  Command
    { name         = "init"
    , header       = "tsundoku init - initialize a pile"
    , description  = "Create a new and empty pile."
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
    (False, False) -> writePile path Pile.empty >> return (Success Nothing Nothing)
    (True,  False) -> failWith "a pile already exists"
    (True, True)   -> writePile path Pile.empty >> succeedWith "the pile was reset"
    (False, True)  -> do
      writePile path Pile.empty
      succeedWith "there's no pile to reset, so a new one was made"
