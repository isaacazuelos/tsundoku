-- |
-- Module      : Tsundoku.Book
-- Description : Our book type
-- License     : MIT License
-- Maintainer  : Isaac Azuelos

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Tsundoku.Book
    ( -- * Book metadata
      Book
      ( Book
      , firstName
      , lastName
      , otherAuthors
      , title
      , published
      , status
      , tags )
    -- * Reader status
    , Status
      ( Unread
      , Read
      , Abandoned
      , started
      , finished
      , abandoned
      , place )
    -- * Type aliases
    , Tag
    , Title
    )
  where

import qualified Data.Text          as Text
import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Clock    as Clock

import           Data.Aeson         (FromJSON, ToJSON)
import           GHC.Generics       (Generic)

-- | Books are the core entry in our pile.
data Book
  = Book
    { firstName    :: Text.Text
    , lastName     :: Text.Text
    , otherAuthors :: Text.Text
    , title        :: Title
    , published    :: Maybe Calendar.Day
    , status       :: Status
    , tags         :: [Tag] }
  deriving (Show, Eq, Generic)

instance Ord Book where
  compare b1 b2 = compare b1comp b2comp
    where
      b1comp = (lastName b1, firstName b1, title b1)
      b2comp = (lastName b2, firstName b2, title b2)

instance FromJSON Book
instance ToJSON Book

-- | A book's status tracks if the book has been read, or even attempted. The
-- dates are optional, since tsundoku could still be used reasonably without
-- tracking dates.
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
  deriving (Show, Eq, Generic)

instance FromJSON Status
instance ToJSON Status

-- | Tags are just _any_ text. I didn't see a benefit to making them
-- need to be lowercase, or not have whitespace, or whatever.
type Tag = Text.Text

-- | Books have titles, which are just text.
type Title = Text.Text
