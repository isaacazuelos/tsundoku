module Main where

import           Prelude             hiding (putStrLn)

import           Control.Monad
import           Data.Text.IO
import           Options.Applicative
import           System.Exit

import           Tsundoku.IO         (pilePath, writePile)
import qualified Tsundoku.Verb       as Verb
import qualified Tsundoku.Verb.Init  as Init (verb)

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

commands = helper <*> subparser (mconcat [ Verb.command Init.verb ])
