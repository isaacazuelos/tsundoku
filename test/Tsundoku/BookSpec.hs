{-# LANGUAGE OverloadedStrings #-}

module Tsundoku.BookSpec
    ( spec )
  where

import           Test.Hspec

import qualified Tsundoku.Book as Book

spec :: Spec
spec =
  describe "book" $
    it "doesn't have any associated functions yet" $
      True `shouldSatisfy` id
  
