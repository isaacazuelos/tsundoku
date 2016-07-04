module Main where

import           Prelude              hiding (putStrLn)

import           Control.Monad
import           Data.Text.IO
import           Options.Applicative
import           System.Exit

import           Tsundoku.IO          (pilePath, writePile)

import qualified Tsundoku.Verb        as Verb


import qualified Tsundoku.Verb.Add    as Add (verb)
import qualified Tsundoku.Verb.Init   as Init (verb)
import qualified Tsundoku.Verb.List   as List (verb)
import qualified Tsundoku.Verb.Remove as Remove (verb)
import qualified Tsundoku.Verb.Path as Path (verb)

main :: IO ()
main = do result <- join $ execParser (info commands idm)
          case result of
            Verb.Failure err -> putStrLn err >> exitFailure
            Verb.Success Nothing Nothing -> exitSuccess
            Verb.Success (Just message) Nothing -> putStrLn message
            Verb.Success message (Just pile) -> do
              path <- pilePath
              writePile path pile
              forM_ message putStrLn
              exitSuccess

commands = helper <*> subparser (mconcat
  [ Verb.command Add.verb
  , Verb.command Init.verb
  , Verb.command Remove.verb
  , Verb.command List.verb
  , Verb.command Path.verb
  ])
