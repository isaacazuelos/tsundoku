-- |
-- Module      : Tsundoku.Verb.Status
-- Description : Verbs for changing a book's status
-- License     : MIT License
-- Maintainer  : Isaac Azuelos

{-# LANGUAGE OverloadedStrings #-}

module Tsundoku.Command.Status ( start, finish, abandon, unread ) where

import qualified Data.Char           as Char (isDigit)
import           Data.Maybe          (fromMaybe, isJust)
import qualified Data.Semigroup      as Semi
import qualified Data.Text           as Text
import qualified Data.Time.Calendar  as Calendar
import qualified Data.Time.LocalTime as Time
import           Options.Applicative hiding (Success, action, header, command)
import           System.Directory

import qualified Tsundoku.Book       as Book
import           Tsundoku.IO
import Tsundoku.Pile
import           Tsundoku.Command

data Date
  = Specifically Calendar.Day
  | Unspecified
  | None
  deriving (Show, Eq)

(<!>) :: Maybe Calendar.Day -> Date -> Maybe Calendar.Day
(<!>) _ None = Nothing
(<!>) _ (Specifically d)= Just d
(<!>) d Unspecified = d

readDate :: String -> ReadM Date
readDate "none"  = return None
readDate date@[y1, y2, y3, y4, '-', m1, m2, '-', d1, d2] = do
    let int s = if all Char.isDigit s
                  then return $ read s
                  else readerError $ "invalid date: " ++ date
    year  <- int [y1, y2, y3, y4]
    month <- int [m1, m2]
    day   <- int [d1, d2]
    case Calendar.fromGregorianValid year month day of
      Nothing   -> readerError $ date ++ " is not a real date."
      Just day -> return (Specifically day)
readDate _ = readerError "Expecting date format yyyy-mm-dd."

getCurrentDay :: IO Calendar.Day
getCurrentDay = localDayFromZonedTime <$> Time.getZonedTime
  where localDayFromZonedTime = Time.localDay . Time.zonedTimeToLocalTime

type StatusAction opts
  = opts -> Pile -> Calendar.Day -> Either PileError Pile

statusAction :: StatusAction opts -> opts -> IO Result
statusAction pureAction options = do
  pile  <- pilePath >>= readPile
  today <- getCurrentDay
  let pile' = pureAction options pile today
  case pile' of
    Right p -> return $ Success Nothing (Just p)
    Left NoSuchBookError -> failWith "No book with that title was found."
    Left err -> failWith $ "unexpected error: " <> Text.pack (show err)

start :: Command StartOptions
start =
  Command
    { name         = "start"
    , header       = "tsundoku start - mark a book as started"
    , description  = "Mark a book as started."
    , optionParser = startParser
    , action       = startAction
    }

data StartOptions
  = StartOptions Text.Text Date
  deriving (Show, Eq)

startParser :: Parser StartOptions
startParser = StartOptions
  <$> argument (Text.pack <$> str)
    (metavar "TITLE"
    <> help "The title of the book")
  <*> option (str >>= readDate)
    (metavar "DATE"
    <> long "started"
    <> value Unspecified
    <> short 's'
    <> help "the date you started the book")

startAction :: StartOptions -> IO Result
startAction = statusAction startPure

startPure :: StatusAction StartOptions
startPure (StartOptions title started) pile today = do
  book        <- find title pile
  let status' = Book.Started $ case started of
                                Specifically d -> Just d
                                Unspecified    -> Just today
                                None           -> Nothing
  let book'   = book { Book.status = status' }
  pileWithout <- delete title pile
  add book' pileWithout

finish :: Command FinishOptions
finish =
  Command
    { name         = "finish"
    , header       = "tsundoku finish - mark a book as finished"
    , description  = "Mark that you've finished a book."
    , optionParser = finishParser
    , action       = finishAction
    }

data FinishOptions = Fin Text.Text Date Date deriving (Show, Eq)

finishParser :: Parser FinishOptions
finishParser = Fin
  <$> argument (Text.pack <$> str)
    (metavar "TITLE"
    <> help "The title of the book")
  <*> option (str >>= readDate)
    (metavar "DATE"
    <> long "started"
    <> short 's'
    <> value Unspecified
    <> help "the date you started the book")
  <*> option (str >>= readDate)
    (metavar "DATE"
    <> long "finished"
    <> short 'f'
    <> value Unspecified
    <> help "the date you finished the book")

finishAction :: FinishOptions -> IO Result
finishAction = statusAction pureFinish

pureFinish :: FinishOptions -> Pile -> Calendar.Day -> Either PileError Pile
pureFinish (Fin title started finished) pile today = do
    book        <- find title pile
    let status' = updatedStatus (Book.status book)
    let book'   = book { Book.status = status' }
    pileWithout <- delete title pile
    add book' pileWithout
  where
    updatedStatus status = let t = Just today in
      case status of
        Book.Unread -> Book.Finished (Nothing <!> started) (t <!> finished)
        Book.Started s -> Book.Finished (s <!> started) (t <!> finished)
        Book.Finished s f -> Book.Finished (s <!> started) ((f <|> t) <!> finished)
        Book.Abandoned s a p -> Book.Finished (s <!> started) ((a <|> t) <!> finished)

abandon :: Command AbandonOptions
abandon =
  Command
    { name         = "abandon"
    , header       = "tsundoku abandon - give up"
    , description  = "Abandon books that aren't worth finishing."
    , optionParser = abandonParser
    , action       = abandonAction
    }

data AbandonOptions
  = AbandonOptions Text.Text Date Date (Maybe Text.Text)
  deriving (Show, Eq)

abandonParser :: Parser AbandonOptions
abandonParser = AbandonOptions
  <$> argument (Text.pack <$> str)
    (metavar "TITLE"
    <> help "The title of the book")
  <*> option (str >>= readDate)
    (long "started"
    <> short 's'
    <> value Unspecified
    <> help "the date you started the book"
    <> metavar "DATE")
  <*> option (str >>= readDate)
    (long "finished"
    <> short 'f'
    <> value Unspecified
    <> help "the date you put the book down"
    <> metavar "DATE")
  <*> optional (option (Text.pack <$> str)
    (long "place"
    <> short 'p'
    <> help "a reminder of where you stopped"
    <> metavar "PLACE"))

abandonAction :: AbandonOptions -> IO Result
abandonAction = statusAction abandonPure

abandonPure :: AbandonOptions -> Pile -> Calendar.Day -> Either PileError Pile
abandonPure (AbandonOptions title started abandoned place) pile today = do
  book        <- find title pile
  let t = Just today
  let status' = case Book.status book of
                  Book.Unread ->
                    Book.Abandoned (Nothing <!> started) (t <!> abandoned) place
                  Book.Started s ->
                    Book.Abandoned (s <!> started) (t <!> abandoned ) place
                  Book.Finished s f ->
                    Book.Abandoned (s <!> started) ((f <|> t) <!> abandoned) place
                  Book.Abandoned s a p ->
                    Book.Abandoned (s <!> started) ((a <|> t) <!> abandoned) (place <|> p)
  let book'   = book { Book.status = status' }
  pileWithout <- delete title pile
  add book' pileWithout

unread :: Command UnreadOptions
unread = Command
  { name         = "unread"
  , header       = "tsundoku unread - unread a book"
  , description  = "Mark a book as unread."
  , optionParser = unreadParser
  , action       = unreadAction
  }

data UnreadOptions = UnreadOptions Text.Text deriving (Show, Eq)

unreadParser :: Parser UnreadOptions
unreadParser = UnreadOptions
  <$> argument (Text.pack <$> str)
    (metavar "TITLE"
    <> help "The title of the book")

unreadAction :: UnreadOptions -> IO Result
unreadAction = statusAction unreadPure

unreadPure :: UnreadOptions -> Pile -> Calendar.Day -> Either PileError Pile
unreadPure (UnreadOptions title) pile today = do
  book        <- find title pile
  let book'   = book { Book.status = Book.Unread }
  pileWithout <- delete title pile
  add book' pileWithout
