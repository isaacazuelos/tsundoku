-- |
-- Module      : Verb.List
-- Description : The verb that lists your books.
-- License     : MIT License
-- Maintainer  : Isaac Azuelos

{-# LANGUAGE OverloadedStrings #-}

module Tsundoku.Verb.List (verb) where

import           Data.List           (sortBy)
import           Data.Ord            (comparing)
import qualified Data.Text           as Text
import           Options.Applicative hiding (Success, action, header)
import           System.Directory

import qualified Tsundoku.Book       as Book
import           Tsundoku.IO
import qualified Tsundoku.Pile       as Pile
import           Tsundoku.Verb

-- | The init verb.
verb =
  Verb
    { name         = "list"
    , header       = "tsundoku list - list your books"
    , description  = "list the books in your pile"
    , optionParser = listOptionParser
    , action       = listAction }

-- | list's options. See the docs for a human-readable explination.
data Options
  = Options
    { sortAlphaberically :: Bool
    , status             :: Maybe Status
    , tag                :: Maybe Book.Tag }
  deriving (Show, Eq)

data Status = Unread | Read | Abandoned deriving (Show, Eq)

-- | An applicative parser for our options.
listOptionParser :: Parser Options
listOptionParser = Options
  <$> switch (short 'a' <> long "alphabetical" <> help "sort alphabetically")
  <*> optional (option (str >>= statusize) (metavar "status" <> long "status" <> short 's' <> help "filter by status"))
  <*> optional (option (Text.pack <$> str) (metavar "tag" <> long "tag" <> short 't' <> help "limit results to tag"))

statusize :: String -> ReadM Status
statusize "r"         = return Read
statusize "read"      = return Read
statusize "a"         = return Abandoned
statusize "abandoned" = return Abandoned
statusize "u"         = return Unread
statusize "unread"    = return Unread
statusize str         = readerError
  $  "invalid status: '"
  <> str
  <> "', use one of 'read', 'unread', 'abandoned'."

-- | The entry point into our action, taking the options.
listAction :: Options -> IO Result
listAction options = do
  path <- pilePath
  pile <- readPile path
  if pile == Pile.empty
    then succeedWith "Your pile is empty. :("
    else do
      let allBooks = Pile.list pile
      let taggedBooks = filter (`hasTag` tag options) allBooks
      let books = filter (`hasStatus` status options) taggedBooks
      let sortedBooks = if sortAlphaberically options
                          then sortBy (comparing sortables) books
                          else books
      mapM_ (putStrLn . pretty) sortedBooks
      return Success { message = Nothing, updated = Nothing}

sortables :: Book.Book -> (Maybe Text.Text, Maybe Text.Text, Book.Title)
sortables book = (Book.lastName book, Book.firstName book, Book.title book)

hasTag :: Book.Book -> Maybe Book.Tag -> Bool
hasTag _ Nothing = True
hasTag book (Just tag) = tag `elem` Book.tags book

pretty :: Book.Book -> String
pretty book = "___"

hasStatus :: Book.Book -> Maybe Status -> Bool
hasStatus _ Nothing = True
hasStatus Book.Book {Book.status = Book.Read {}} (Just Read) = True
hasStatus Book.Book {Book.status = Book.Abandoned {}} (Just Abandoned) = True
hasStatus Book.Book {Book.status = Book.Unread {}} (Just Unread) = True
hasStatus _ _ = False
