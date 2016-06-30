-- |
-- Module      : Tsundoku.Pile
-- Description : Our book pile
-- License     : MIT License
-- Maintainer  : Isaac Azuelos

{-# LANGUAGE OverloadedStrings #-}

module Tsundoku.Pile
  ( Pile
  , empty
  , add
  , delete
  , find
  , list
  , PileError (NoSuchBookError, BookNotUniqueError)
  ) where

import qualified Tsundoku.Book as Book

data Pile = Pile [Book.Book] deriving (Show, Eq)

data PileError
  = NoSuchBookError
  | BookNotUniqueError
    deriving (Show, Eq)

empty :: Pile
empty = Pile []

list :: Pile -> [Book.Book]
list (Pile books) = books

add :: Book.Book -> Pile -> Either PileError Pile
add book pile@(Pile books) =
  case find (Book.title book) pile of
    Left NoSuchBookError -> Right (Pile (book:books))
    _ -> Left BookNotUniqueError

delete :: Book.Title -> Pile -> Either PileError Pile
delete title pile@(Pile books) =
  case find title pile of
    Right _ -> Right $ Pile (filter (not . (`hasTitle` title)) books)
    Left err -> Left err

hasTitle :: Book.Book -> Book.Title -> Bool
hasTitle book title = Book.title book == title

find :: Book.Title -> Pile -> Either PileError Book.Book
find title pile@(Pile books) =
  case filter (`hasTitle` title) books of
    []     -> Left NoSuchBookError
    [book] -> Right book
    _      -> Left BookNotUniqueError
