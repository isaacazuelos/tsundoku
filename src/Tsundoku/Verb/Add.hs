-- |
-- Module      : Verb.Add
-- Description : The verb that adds books
-- License     : MIT License
-- Maintainer  : Isaac Azuelos

{-# LANGUAGE OverloadedStrings #-}

module Tsundoku.Verb.Add (verb) where

import           Prelude             hiding (last)

import           Options.Applicative hiding (Success, action, header)
import           System.Directory

import qualified Data.Char           as Char (isDigit)
import qualified Data.Text           as Text
import qualified Data.Time.Calendar  as Calendar

import           Tsundoku.IO
import           Tsundoku.Verb

import qualified Tsundoku.Book       as Book
import qualified Tsundoku.Pile       as Pile

-- | The init verb.
verb =
  Verb
    { name         = "add"
    , header       = "tsundoku add - add books to your pile"
    , description  = "add a book to your pile"
    , optionParser = addParser
    , action       = add
    }

-- | Add's options. Check the docs for deets.
data AddOptions
  = AddOptions
    { title     :: Text.Text
    , first     :: Maybe Text.Text
    , last      :: Maybe Text.Text
    , others    :: Maybe Text.Text
    , published :: Maybe Integer
    , tags      :: [Book.Tag]
    }
  deriving (Show, Eq)

-- | An applicative parser for our options.
addParser :: Parser AddOptions
addParser = AddOptions
  <$> argument (Text.pack <$> str)
    (metavar "title"
    <> help "The book's unique title")
  <*> optional (option (Text.pack <$> str)
      (metavar "firstname"
        <> long "firstname"
        <> short 'f'
        <> help "The author's first name"))
  <*> optional (option (Text.pack <$> str)
    (metavar "lastname"
    <> long "lastname"
    <> short 'l'
    <> help "The author's last name"))
  <*> optional (option (Text.pack <$> str)
    (metavar "otherauthors"
    <> long "otherauthors"
    <> short 'o'
    <> help "Other author names"))
  <*> optional (option (str >>= yearParser)
    (metavar "year"
    <> long "published"
    <> short 'p'
    <> help "The year the book was published"))
  <*> many (option (Text.pack <$> str)
    (metavar "tag"
    <> short 't'
    <> long "tag"
    <> help "Add tags to the book"))

-- | Parse a year, i.e. make sure all the characters are digits.
yearParser :: String -> ReadM Book.Year
yearParser s = if all Char.isDigit s
  then return $ read s
  else readerError "invalid year: year is not a number"

-- | The entry point into our action, taking the options.
add :: AddOptions -> IO Result
add options = do
  path <- pilePath
  pile <- readPile path
  let book = fromOptions options
  case Pile.add book pile of
    Left Pile.BookNotUniqueError -> failWith "A book with that title exists."
    Left err -> failWith $ "unexpected error: " <> Text.pack (show err)
    Right pile' -> return Success { message = Nothing, updated = Just pile' }

fromOptions :: AddOptions -> Book.Book
fromOptions options
  = Book.Book
      { Book.firstName    = first options
      , Book.lastName     = last options
      , Book.otherAuthors = others options
      , Book.title        = title options
      , Book.published    = published options
      , Book.status       = Book.Unread
      , Book.tags         = tags options
      }
