-- |
-- Module      : Verbs.Verbs
-- Description : Infrastructure for building verbs.
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- This module is a bunch of common ground that is used to build our verbs.
-- Each individual verb must export just a single 'Verb' record, which gets
-- built into a subcommand and run to yeild a 'Result'.

{-# LANGUAGE OverloadedStrings #-}

module Tsundoku.Verb where

import           Data.Monoid         ((<>))
import qualified Data.Text           as Text
import qualified Options.Applicative as Options

import qualified Tsundoku.Pile       as Pile

-- | A command is an options parser, and the action to do afterwords.
-- Every verb should export just it's `command :: Command`

-- | The result of a pure command. Either there's an error message to print,
-- or there's some results to print and update the store with.
data Result
  = Failure Text.Text
  | Success
    { message :: Maybe Text.Text
    , updated :: Maybe Pile.Pile }
  deriving (Show, Eq)

-- | Succeed with a specific message. The signature here is designed to make
-- it easy to use by appending a @>> succeedWith "message"@ to the end of some
-- other IO action.
succeedWith :: Text.Text -> IO Result
succeedWith msg = return $ Success (Just msg) Nothing

-- | Fail with a specific message. The signature here is designed like
-- 'succeedWith'.
failWith :: Text.Text -> IO Result
failWith msg = return $ Failure msg

-- | The goal is to make a Verb for each little action we want to implement, and
-- have infrastructure to build out the rest of the UI from that.
data Verb options
  = Verb
    { name :: String
    , header       :: String
    , description  :: String
    , optionParser :: Options.Parser options
    , action       :: options -> IO Result
    }

-- | These are the command types used by Options.subparser to bulid our root
-- parser.
type Command = Options.Mod Options.CommandFields (IO Result)

-- | Combine an Verb and a parser for its options to build a Command.
command :: Verb a  -> Command
command verb = Options.command (name verb) $ Options.info parser info
 where
    parser = Options.helper <*> (action verb <$> optionParser verb)
    info = Options.fullDesc <>
           Options.progDesc (description verb) <>
           Options.header (header verb)
