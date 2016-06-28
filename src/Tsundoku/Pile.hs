-- |
-- Module      : Tsundoku.Pile
-- Description : Our book pile
-- License     : MIT License
-- Maintainer  : Isaac Azuelos

{-# LANGUAGE OverloadedStrings #-}

module Tsundoku.Pile
  ( empty
  , add
  , delete
  , find
  , list
  , PileError (NoSuchBookError, BookNotUniqueError)
  ) where

import qualified Tsundoku.Book as Book

data Pile = Pile deriving (Show, Eq)

data PileError
  = NoSuchBookError
  | BookNotUniqueError
    deriving (Show, Eq)

empty :: Pile
empty = error "empty unimplemented"

list :: Pile -> [Book.Book]
list = error "list unimplemented"

add :: Book.Book -> Pile -> Either PileError Pile
add = error "add unimplemented"

delete :: Book.Title -> Pile -> Either PileError Pile
delete = error "delete unimplemented"

find :: Book.Title -> Pile -> Either PileError Book.Book
find = error "fine unimplemented"
