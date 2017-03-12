-- |
-- Module      : Tsundoku.Pretty
-- Description : Pretty printing books
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- This module is in charge of pretty printing books.
--
-- Right now, the goal is a format like the line below

{-# LANGUAGE OverloadedStrings #-}

module Tsundoku.Pretty ( pretty ) where

import           Prelude            hiding (null, show)
import qualified Prelude            (null, show)

import           Data.Maybe         (fromMaybe)
import           Data.Monoid        ((<>))

import           Data.Text
import qualified Data.Time.Calendar as Calendar
import qualified Tsundoku.Book      as Book

show = pack . Prelude.show

newtype PrettyOptions
  = PrettyOptions
    { showTags :: Bool }

pretty :: Bool -> Book.Book -> Text
pretty showTags b = fromMaybe "" (Book.firstName b)
  `space`  fromMaybe "" (Book.lastName  b)
  `comma`  fromMaybe "" (Book.otherAuthors b)
  `hyphen` Book.title b
  `space`  (parens . maybe "" show . Book.published) b
  `space`  status b
  `space`  if showTags then tags b else mempty

status :: Book.Book -> Text
status b = case Book.status b of
  Book.Abandoned s f p -> "abandoned" `space` maybe "" ("at " <>) p
  Book.Finished  s f   -> "finished" `space` prefix "on " (date f)
  Book.Started   s     -> "started" `space` date s
  Book.Unread          -> "unread"

date :: Maybe Calendar.Day -> Text
date Nothing  = ""
date (Just d) = pack (Calendar.showGregorian d)

tags :: Book.Book -> Text
tags b = braces $ intercalate ", " ts
  where ts = Book.tags b

braces :: Text -> Text
braces t = if null t then mempty else "[" <> t <> "]"

parens :: Text -> Text
parens t = if null t then mempty else "(" <> t <> ")"

space :: Text -> Text -> Text
space = joinWith " "

comma :: Text -> Text -> Text
comma = joinWith ", "

hyphen :: Text -> Text -> Text
hyphen = joinWith " - "

prefix :: Text -> Text -> Text
prefix _ "" = ""
prefix p s  = p <> s

joinWith :: Text -> Text -> Text -> Text
joinWith joiner s1 s2
  | null s1 = s2
  | null s2 = s1
  | otherwise = s1 <> joiner <> s2
