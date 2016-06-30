-- |
-- Module      : Tsundoku.IO
-- Description : Basic shared IO operations
-- License     : MIT License
-- Maintainer  : Isaac Azuelos

{-# LANGUAGE OverloadedStrings #-}

module Tsundoku.IO
  ( pilePath
  , readPile
  , writePile
  ) where

import qualified Tsundoku.Pile as Pile

pilePath :: IO FilePath
pilePath = error "pilePath unimplemented"

readPile :: FilePath -> IO Pile.Pile
readPile = error "readPile unimplemented"

writePile :: FilePath -> Pile.Pile -> IO ()
writePile = error "writePile unimplemented"
