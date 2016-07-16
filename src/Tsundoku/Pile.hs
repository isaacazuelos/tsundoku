-- |
-- Module      : Tsundoku.Pile
-- Description : Our book pile
-- License     : MIT License
-- Maintainer  : Isaac Azuelos

{-# LANGUAGE DeriveGeneric #-}
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

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

import qualified Tsundoku.Book as Book

-- | Our Pile is just our stack of books, with the most recetnly touched
-- book on top.
data Pile = Pile [Book.Book] deriving (Show, Eq, Generic)

instance FromJSON Pile
instance ToJSON Pile

-- | Piles can only really fail in two ways. Either the book exists when it
-- shouldn't, or doesn't when it should.
data PileError
  = NoSuchBookError
  | BookNotUniqueError
    deriving (Show, Eq)

-- | An empty pile with no books in it. How sad.
empty :: Pile
empty = Pile []

-- | Get a list of the books in the pile, sorted with the most recently touched
-- on top.
list :: Pile -> [Book.Book]
list (Pile books) = books

-- | Add a book to the top of the pile. This can PileError if there's already a
-- book with that title in the pile.
add :: Book.Book -> Pile -> Either PileError Pile
add book pile@(Pile books) =
  case find (Book.title book) pile of
    Left NoSuchBookError -> Right (Pile (book:books))
    _ -> Left BookNotUniqueError

-- | Delete a book from the pile, removing it entierly. This will PileError if
-- there's no book matching the given title.
delete :: Book.Title -> Pile -> Either PileError Pile
delete title pile@(Pile books) =
  case find title pile of
    Right _ -> Right $ Pile (filter (not . (`hasTitle` title)) books)
    Left err -> Left err

-- | Do two books have the same title?
hasTitle :: Book.Book -> Book.Title -> Bool
hasTitle book title = Book.title book == title

-- | Find a book by its title in the pile. This will PileError if there's
-- no such book.
find :: Book.Title -> Pile -> Either PileError Book.Book
find title pile@(Pile books) =
  case filter (`hasTitle` title) books of
    []     -> Left NoSuchBookError
    [book] -> Right book
    _      -> Left BookNotUniqueError
