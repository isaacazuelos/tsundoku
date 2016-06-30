-- |
-- Module      : Tsundoku.IO
-- Description : Basic shared IO operations
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- Right now, any IO exceptions are uncaught.

{-# LANGUAGE OverloadedStrings #-}

module Tsundoku.IO
  ( pilePath
  , readPile
  , writePile
  ) where

import qualified Data.Aeson           as JSON
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text            as Text

import           System.Directory     (getHomeDirectory)
import           System.Exit
import           System.FilePath      ((<.>), (</>))

import qualified Tsundoku.Pile        as Pile

-- | For now, it just looks for a tsundoku file (@~/.tsundoku.json@)
-- in you home directory.
pilePath :: IO FilePath
pilePath = (</> ".tsundoku" <.> ".json") <$> getHomeDirectory

-- | Reads the pile in from the path. If anything goes wrong, it'll throw
-- exceptions.
readPile :: FilePath -> IO Pile.Pile
readPile path = do
  contents <- ByteString.readFile path
  case JSON.eitherDecode contents of
    Right pile -> return pile
    Left  err  -> error err

-- | Write out a pile as JSON to the path. This will create a file at the path
-- if needed.
writePile :: FilePath -> Pile.Pile -> IO ()
writePile path pile = ByteString.writeFile path (JSON.encode pile)
