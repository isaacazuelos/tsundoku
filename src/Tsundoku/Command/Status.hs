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
import           Data.Time.Calendar  as Calendar
import qualified Data.Time.LocalTime as Time
import           Options.Applicative hiding (Success, action, header, command)
import           System.Directory

import qualified Tsundoku.Book       as Book
import           Tsundoku.IO
import qualified Tsundoku.Pile       as Pile
import           Tsundoku.Command

readDate :: String -> ReadM Calendar.Day
readDate date@[y1, y2, y3, y4, '-', m1, m2, '-', d1, d2] = do
    let int s = if all Char.isDigit s
                  then return $ read s
                  else readerError $ "invalid date: " ++ date
    year  <- int [y1, y2, y3, y4]
    month <- int [m1, m2]
    day   <- int [d1, d2]
    case fromGregorianValid year month day of
      Nothing   -> readerError $ date ++ " is not a real date."
      Just day -> return day
readDate _ = readerError "Expecting date format yyyy-mm-dd."


getCurrentDay :: IO Calendar.Day
getCurrentDay = localDayFromZonedTime <$> Time.getZonedTime
  where localDayFromZonedTime = Time.localDay . Time.zonedTimeToLocalTime

statusAction :: (opts -> Pile.Pile -> Day -> Either Pile.PileError Pile.Pile) -> opts -> IO Result
statusAction pureAction options = do
  pile  <- pilePath >>= readPile
  today <- getCurrentDay
  let pile' = pureAction options pile today
  case pile' of
    Right p -> return $ Success Nothing (Just p)
    Left Pile.NoSuchBookError -> failWith "No book with that title was found."
    Left err -> failWith $ "unexpected error: " <> Text.pack (show err)

(<!>) :: Maybe a -> Maybe a -> Maybe a
(<!>) a@(Just _) b = a
(<!>) Nothing  b = b


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
  = StartOptions Text.Text (Maybe Calendar.Day)
  deriving (Show, Eq)

startParser :: Parser StartOptions
startParser = StartOptions
  <$> argument (Text.pack <$> str)
    (metavar "TITLE"
    <> help "The title of the book")
  <*> optional (option (str >>= readDate)
    (long "started"
    <> short 's'
    <> help "the date you started the book"))

startAction :: StartOptions -> IO Result
startAction = statusAction startPure

startPure :: StartOptions -> Pile.Pile -> Calendar.Day -> Either Pile.PileError Pile.Pile
startPure (StartOptions title started) pile today = do
  book        <- Pile.find title pile
  let status' = Book.Started (Just (fromMaybe today started))
  let book'   = book { Book.status = status' }
  pileWithout <- Pile.delete title pile
  Pile.add book' pileWithout

finish :: Command FinishOptions
finish =
  Command
    { name         = "finish"
    , header       = "tsundoku finish - mark a book as finished"
    , description  = "Mark that you've finished a book."
    , optionParser = finishParser
    , action       = finishAction
    }

data FinishOptions =
  Fin Text.Text (Maybe Calendar.Day) (Maybe Calendar.Day) deriving (Show, Eq)

finishParser :: Parser FinishOptions
finishParser = Fin
  <$> argument (Text.pack <$> str)
    (metavar "TITLE"
    <> help "The title of the book")
  <*> optional (option (str >>= readDate)
    (long "started"
    <> short 's'
    <> help "the date you started the book"))
  <*> optional (option (str >>= readDate)
    (long "finished"
    <> short 'f'
    <> help "the date you finished the book"))

finishAction :: FinishOptions -> IO Result
finishAction = statusAction pureFinish

pureFinish :: FinishOptions -> Pile.Pile -> Calendar.Day -> Either Pile.PileError Pile.Pile
pureFinish (Fin title started finished) pile today = do
  book        <- Pile.find title pile
  let status' = case Book.status book of
                  Book.Unread          -> Book.Finished  started        (finished <!> Just today)
                  Book.Started s       -> Book.Finished (started <!> s) (finished <!> Just today)
                  Book.Finished s f    -> Book.Finished (started <!> s) (finished <!> f)
                  Book.Abandoned s a _ -> Book.Finished (started <!> s) (finished <!> Just today)
  let book'   = book { Book.status = status' }
  pileWithout <- Pile.delete title pile
  Pile.add book' pileWithout

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
  = AbandonOptions Text.Text (Maybe Calendar.Day) (Maybe Calendar.Day) (Maybe Text.Text)
  deriving (Show, Eq)

abandonParser :: Parser AbandonOptions
abandonParser = AbandonOptions
  <$> argument (Text.pack <$> str)
    (metavar "TITLE"
    <> help "The title of the book")
  <*> optional (option (str >>= readDate)
    (long "started"
    <> short 's'
    <> help "the date you started the book"
    <> metavar "YYYY-MM-DD"))
  <*> optional (option (str >>= readDate)
    (long "finished"
    <> short 'f'
    <> help "the date you finished the book"
    <> metavar "YYYY-MM-DD"))
  <*> optional (option (Text.pack <$> str)
    (long "place"
    <> short 'p'
    <> help "a reminder of where you stopped"
    <> metavar "PLACE"))

abandonAction :: AbandonOptions -> IO Result
abandonAction = statusAction abandonPure

abandonPure :: AbandonOptions -> Pile.Pile -> Calendar.Day -> Either Pile.PileError Pile.Pile
abandonPure (AbandonOptions title started abandoned place) pile today = do
  book        <- Pile.find title pile
  let status' = case Book.status book of
                  Book.Unread          -> Book.Abandoned started         (abandoned <!> Just today)       place
                  Book.Started s       -> Book.Abandoned (started <!> s) (abandoned <!> Just today)       place
                  Book.Finished s f    -> Book.Abandoned (started <!> s) (abandoned <!> f <!> Just today) place
                  Book.Abandoned s a p -> Book.Abandoned (started <!> s) (abandoned <!> a)                (place <!> p)
  let book'   = book { Book.status = status' }
  pileWithout <- Pile.delete title pile
  Pile.add book' pileWithout

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

unreadPure :: UnreadOptions -> Pile.Pile -> Calendar.Day -> Either Pile.PileError Pile.Pile
unreadPure (UnreadOptions title) pile today = do
  book        <- Pile.find title pile
  let book'   = book { Book.status = Book.Unread }
  pileWithout <- Pile.delete title pile
  Pile.add book' pileWithout
