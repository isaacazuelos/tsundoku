-- |
-- Module      : Tsundoku.Command.List
-- Description : The verb that lists your books.
-- License     : MIT License
-- Maintainer  : Isaac Azuelos

{-# LANGUAGE OverloadedStrings #-}

module Tsundoku.Command.List (command) where

import           Prelude             hiding (show)
import qualified Prelude             (show)

import           Data.List           (intercalate, sortBy)
import           Data.Ord            (comparing)
import qualified Data.Text           as Text
import qualified Data.Time.Calendar  as Calendar
import           Options.Applicative hiding (Success, action, header, command)
import           System.Directory


import qualified Tsundoku.Book       as Book
import           Tsundoku.IO
import           Tsundoku.Pretty
import qualified Tsundoku.Pile       as Pile
import           Tsundoku.Command

-- | The list command lists the books in the pile.
command =
  Command
    { name         = "list"
    , header       = "tsundoku list - list your books"
    , description  = "List the books in your pile."
    , optionParser = listOptionParser
    , action       = listAction }

-- | list's options. See the docs for a human-readable explination.
data Options
  = Options
    { sortAlphaberically :: Bool
    , showTags           :: Bool
    , status             :: Maybe Status
    , tag                :: Maybe Book.Tag }
  deriving (Show, Eq)

data Status = Unread | Started | Finished | Abandoned deriving (Show, Eq)

-- | An applicative parser for our options.
listOptionParser :: Parser Options
listOptionParser = Options
  <$> switch
    (short 'l'
    <> long "sort"
    <> help "sort by lastname, firstname, title")
  <*> switch
    (short 'a'
    <> long "alltags"
    <> help "show all tags for every book")
  <*> optional (option (str >>= statusize)
    (metavar "STATUS"
    <> long "status"
    <> short 's'
    <> help "filter by status"))
  <*> optional (option (Text.pack <$> str)
    (metavar "TAG"
    <> long "tag"
    <> short 't'
    <> help "limit results to tag"))

statusize :: String -> ReadM Status
statusize "a"         = return Abandoned
statusize "abandoned" = return Abandoned
statusize "f"         = return Finished
statusize "finished"  = return Finished
statusize "s"         = return Started
statusize "started"   = return Started
statusize "u"         = return Unread
statusize "unread"    = return Unread
statusize str         = readerError $
  "'"
   <> str
   <> "' is an invalid status, use one of "
   <> "'unread', 'started', 'finihsed', or 'abandoned'"

-- | The entry point into our action, taking the options.
listAction :: Options -> IO Result
listAction options = do
  path <- pilePath
  pile <- readPile path
  if pile == Pile.empty
    then succeedWith "your pile is empty"
    else do
      let allBooks = Pile.list pile
      let taggedBooks = filter (`hasTag` tag options) allBooks
      let books = filter (`hasStatus` status options) taggedBooks
      let sortedBooks = if sortAlphaberically options
                          then sortBy (comparing sortables) books
                          else books
      mapM_ (putStrLn . Text.unpack . pretty (showTags options)) sortedBooks
      return Success { message = Nothing, updated = Nothing}

sortables :: Book.Book -> (Maybe Text.Text, Maybe Text.Text, Book.Title)
sortables book = (Book.lastName book, Book.firstName book, Book.title book)

hasTag :: Book.Book -> Maybe Book.Tag -> Bool
hasTag _ Nothing = True
hasTag book (Just tag) = tag `elem` Book.tags book

hasStatus :: Book.Book -> Maybe Status -> Bool
hasStatus _ Nothing = True
hasStatus Book.Book {Book.status = Book.Finished {}} (Just Finished) = True
hasStatus Book.Book {Book.status = Book.Abandoned {}} (Just Abandoned) = True
hasStatus Book.Book {Book.status = Book.Unread {}} (Just Unread) = True
hasStatus _ _ = False
