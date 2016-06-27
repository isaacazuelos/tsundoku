-- |
-- Module      : Tsundoku.Book
-- Description : Our book type
-- License     : MIT License
-- Maintainer  : Isaac Azuelos

{-# LANGUAGE OverloadedStrings #-}

module Tsundoku.Book
  where

import qualified Data.Text          as Text
import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Clock    as Clock

data Book
  = Book
    { firstName    :: Text.Text
    , lastName     :: Text.Text
    , otherAuthors :: Text.Text
    , title        :: Title
    , published    :: Maybe Calendar.Day
    , status       :: Status
    , tags         :: [Tag] }
  deriving (Show, Eq)

instance Ord Book where
  compare b1 b2 = compare b1comp b2comp
    where
      b1comp = (lastName b1, firstName b1, title b1)
      b2comp = (lastName b2, firstName b2, title b2)

data Status
  = Unread
  | Reading
    { started :: Maybe Calendar.Day }
  | Read
    { started  :: Maybe Calendar.Day
    , finished :: Maybe Calendar.Day }
  | Abandoned
    { started   :: Maybe Calendar.Day
    , abandoned :: Maybe Calendar.Day
    , place     :: Maybe Text.Text }
  deriving (Show, Eq)

type Tag = Text.Text

type Title = Text.Text
