module Main where

import           Prelude                  hiding (putStrLn)

import           Control.Monad
import           Data.Text.IO
import           Options.Applicative
import           System.Exit

import           Tsundoku.IO              (pilePath, writePile)

import qualified Tsundoku.Command         as Command

import qualified Tsundoku.Command.Add     as Add (command)
import qualified Tsundoku.Command.Details as Details (command)
import qualified Tsundoku.Command.Init    as Init (command)
import qualified Tsundoku.Command.List    as List (command)
import qualified Tsundoku.Command.Path    as Path (command)
import qualified Tsundoku.Command.Remove  as Remove (command)
import qualified Tsundoku.Command.Status  as Status (abandon, finish, start,
                                                     unread)
import qualified Tsundoku.Command.Tag     as Tag (command)

main :: IO ()
main = do result <- join $ execParser (info commands idm)
          case result of
            Command.Failure err -> putStrLn err >> exitFailure
            Command.Success Nothing Nothing -> exitSuccess
            Command.Success (Just message) Nothing -> putStrLn message
            Command.Success message (Just pile) -> do
              path <- pilePath
              writePile path pile
              forM_ message putStrLn
              exitSuccess

commands = helper <*> subparser (mconcat
  [ Command.subcommand Add.command
  , Command.subcommand Init.command
  , Command.subcommand Remove.command
  , Command.subcommand List.command
  , Command.subcommand Path.command
  , Command.subcommand Tag.command
  , Command.subcommand Details.command
  , Command.subcommand Status.start
  , Command.subcommand Status.finish
  , Command.subcommand Status.abandon
  , Command.subcommand Status.unread
  ])
