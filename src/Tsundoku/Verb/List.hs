-- |
-- Module      : Verb.List
-- Description : The verb that lists your books.
-- License     : MIT License
-- Maintainer  : Isaac Azuelos

{-# LANGUAGE OverloadedStrings #-}

module Tsundoku.Verb.List (verb) where

import           Prelude             hiding (show)
import qualified Prelude             (show)

import           Data.List           (intercalate, sortBy)
import           Data.Ord            (comparing)
import qualified Data.Text           as Text
import qualified Data.Time.Calendar  as Calendar
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

data Status = Unread | Started | Read | Abandoned deriving (Show, Eq)

-- | An applicative parser for our options.
listOptionParser :: Parser Options
listOptionParser = Options
  <$> switch
    (short 'a'
    <> long "alphabetical"
    <> help "sort alphabetically")
  <*> optional (option (str >>= statusize)
    (metavar "status"
    <> long "status"
    <> short 's'
    <> help "filter by status"))
  <*> optional (option (Text.pack <$> str)
    (metavar "tag"
    <> long "tag"
    <> short 't'
    <> help "limit results to tag"))

statusize :: String -> ReadM Status
statusize "a"         = return Abandoned
statusize "abandoned" = return Abandoned
statusize "r"         = return Read
statusize "read"      = return Read
statusize "s"         = return Started
statusize "started"   = return Started
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
      mapM_ (putStrLn . Text.unpack . pretty) sortedBooks
      return Success { message = Nothing, updated = Nothing}

sortables :: Book.Book -> (Maybe Text.Text, Maybe Text.Text, Book.Title)
sortables book = (Book.lastName book, Book.firstName book, Book.title book)

hasTag :: Book.Book -> Maybe Book.Tag -> Bool
hasTag _ Nothing = True
hasTag book (Just tag) = tag `elem` Book.tags book

-- | Pretty print the information about a book.
pretty :: Book.Book -> Text.Text
-- last, first, others - title (year), status (date, location) tagged: tag*
pretty book = Text.intercalate " " [pnames, ptitle, pyear, pstatus, ptags]
  where
    show :: (Show a) => a -> Text.Text
    show = Text.pack . Prelude.show
    pnames = "authors" ---case (Book.lastName book, Book.firstName book, Book.otherAuthors book) of

    ptitle = Book.title book
    pyear = maybe "" (\ d -> "(" <> show d <> ")") (Book.published book)
    pstatus = case Book.status book of
      Book.Unread                 -> "unread"
      Book.Started Nothing        -> "started"
      Book.Started (Just d)       -> "started (" <> Text.pack (Calendar.showGregorian d) <> ")"
      Book.Read s f -> "read" <> parenthesize "started" "finished" s f
      Book.Abandoned s a p -> "abandoned"
        <> parenthesize "started" "abandoned" s a
        <> maybe "" (" place: " <>) p

    ptags = if null (Book.tags book)
      then ""
      else "tagged: " <> Text.intercalate ", " (Book.tags book)

    parenthesize :: Text.Text -> Text.Text -> Maybe Calendar.Day -> Maybe Calendar.Day -> Text.Text
    parenthesize _ _ Nothing   Nothing   = ""
    parenthesize _ f Nothing   (Just f') = " (" <> f <> " " <> show f' <> ")"
    parenthesize s _ (Just s') Nothing   = " (" <> s <> " " <> show s' <> ")"
    parenthesize _ _ (Just s)  (Just f)  = " (" <> show s <> "-" <> show f  <> ")"

hasStatus :: Book.Book -> Maybe Status -> Bool
hasStatus _ Nothing = True
hasStatus Book.Book {Book.status = Book.Read {}} (Just Read) = True
hasStatus Book.Book {Book.status = Book.Abandoned {}} (Just Abandoned) = True
hasStatus Book.Book {Book.status = Book.Unread {}} (Just Unread) = True
hasStatus _ _ = False
