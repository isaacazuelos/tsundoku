{-# LANGUAGE OverloadedStrings #-}

module Tsundoku.PileSpec ( spec ) where

import           Test.Hspec

import Data.Either (isRight)

import Tsundoku.Book
import qualified Tsundoku.Pile as Pile

testBook1 :: Book
testBook1 =
  Book
    { firstName    = Just "Kurt"
    , lastName     = Just "Vonnegut"
    , otherAuthors = Nothing
    , title        = "Slaughterhouse Five"
    , published    = Nothing
    , status       = Finished { started = Nothing , finished = Nothing }
    , tags         = []
    }

testBook2 :: Book
testBook2 =
  Book
    { firstName    = Just "Lois"
    , lastName     = Just "Lane"
    , otherAuthors = Just "Kent Clark, Superman"
    , title        = "Why the World Doesn't Need Superman"
    , published    = Nothing
    , status       = Unread
    , tags         = []
    }

spec :: Spec
spec =
  describe "Pile" $ do
    describe "list" $ do
      it "should produce an empty list for the empty pile" $
        Pile.list Pile.empty `shouldBe` []
      it "should produce the books added to the pile in the order added" $ do
        let Right pile = Pile.add testBook2 Pile.empty >>= Pile.add testBook1
        Pile.list pile `shouldBe` [testBook1, testBook2]
    describe "add" $ do
      it "should add books to piles" $
        Pile.add testBook1 Pile.empty `shouldSatisfy` isRight
      it "should BookNotUniqueError if the title is already in the pile" $ do
        let Right pile = Pile.add testBook1 Pile.empty
        let testBook = testBook2 { title = title testBook1 }
        Pile.add testBook pile `shouldBe` Left Pile.BookNotUniqueError
    describe "delete" $ do
      let Right pile1 = Pile.add testBook1 Pile.empty
      let Right pile2 = Pile.add testBook2 pile1
      it "should delete books in the pile" $
        Pile.delete (title testBook1) pile1 `shouldBe` Right Pile.empty
      it "should delete books anywhere in the pile" $ do
        let expected = Pile.add testBook2 Pile.empty
        Pile.delete (title testBook1) pile2 `shouldBe` expected
      it "should preserve the order of the other books" $ do
        let testBook3 = testBook1 { title = "Some new title" }
        let Right pile3 = Pile.add testBook3 pile2
        let result = Pile.list <$> Pile.delete (title testBook2) pile3
        result `shouldBe` Right [testBook3, testBook1]
      it "should NoSuchBookError when there's no such book" $
        Pile.delete (title testBook1) Pile.empty `shouldBe` Left Pile.NoSuchBookError
    describe "find" $ do
      let Right pile1 = Pile.add testBook1 Pile.empty
      it "should find books by their titles" $
        Pile.find (title testBook1) pile1 `shouldBe` Right testBook1
      it "should not find books that aren't there" $
        Pile.find "not an existing title" pile1 `shouldBe` Left Pile.NoSuchBookError
      -- it "should BookNotUniqueError if the title matches more than one book"
      -- but you can't create such a store, if `add` is working correctly.
      -- We can't even build a pile to test this.
