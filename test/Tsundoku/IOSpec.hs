{-# LANGUAGE OverloadedStrings #-}

module Tsundoku.IOSpec ( spec ) where

import           Test.Hspec

import           System.Directory (getHomeDirectory, makeAbsolute, removeFile)
import           System.Exit      (ExitCode, exitFailure)
import           System.FilePath  (equalFilePath)
import           System.IO        (openTempFile)

import qualified Tsundoku.Pile    as Pile

import           Tsundoku.Book
import           Tsundoku.IO

-- Without mocking the IO Monad and futzing with the type signatures of the
-- module a bunch, I don't know of a good way to test these functions. That's
-- why there's the `testfiles` directory with some hand-written example files.
--
-- I'm also not really sure how I want to handle IO exceptions, so they're not
-- really tested for here, just rethrown.
--
-- I think the right thing to do is catch all the way up at `main` and only
-- there print a nicer error and exit, but I'm really not sold on that.
--
-- I guess what I'm saying is this code isn't great, so I'd like to think of
-- this as provisional so I can work on the commands. Hopefully this isn't too
-- long-lived.

testBook :: Book
testBook =
  Book
    { firstName = Just "Kurt"
    , lastName = Just "Vonnegut"
    , otherAuthors = Nothing
    , title = "Slaughterhouse Five"
    , published = Nothing
    , status = Read { started = Nothing, finished = Nothing }
    , tags = [] }

testPile :: Pile.Pile
testPile = (\ (Right p) -> p) $ Pile.add testBook Pile.empty

testPilePath :: FilePath
testPilePath = "./test/testfiles/pile1.json"

testPilePath2 :: FilePath
testPilePath2 = "./test/testfiles/pile2.json"

spec :: Spec
spec =
  describe "tsundoku IO" $ do
    describe "pilePath" $
      it "should just be that file in your home" $ do
        expected <- (++ "/.tsundoku.json") <$> getHomeDirectory
        result   <- pilePath
        result `shouldSatisfy` equalFilePath expected
    describe "readPile" $
      it "should properly read a legal pile" $
        readPile testPilePath `shouldReturn` testPile
    describe "writePile" $
      it "should write a pile properly" $ do
        writePile testPilePath2 testPile
        readPile testPilePath2 `shouldReturn` testPile
        removeFile testPilePath2
