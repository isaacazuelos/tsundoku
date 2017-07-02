module Main where

import           Prelude                  hiding (putStrLn)

import           Data.Monoid              ((<>))
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
import qualified Tsundoku.Command.Bump    as Bump (command)

main :: IO ()
main = do result <- join $ execParser rootCommand
          case result of
            Command.Failure err -> putStrLn err >> exitFailure
            Command.Success Nothing Nothing -> exitSuccess
            Command.Success (Just message) Nothing -> putStrLn message
            Command.Success message (Just pile) -> do
              path <- pilePath
              writePile path pile
              forM_ message putStrLn
              exitSuccess

rootCommand :: ParserInfo (IO Command.Result)
rootCommand = info (helper <*> subcommands)
  (header "doku - keep track of your books"
  <> footer "See the manual for more information.")

subcommands :: Parser (IO Command.Result)
subcommands = subparser $ mconcat
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
  , Command.subcommand Bump.command
  ]
