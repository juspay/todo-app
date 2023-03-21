module Main (main) where

import Data.Aeson (Result (..))
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import qualified Data.Text as DT
import qualified Main.Utf8 as Utf8
import Options.Applicative hiding (Success)
import System.Environment (lookupEnv)
import Text.PrettyPrint.Boxes hiding ((<>))
import Text.Read (readMaybe)
import TodoApp.Request (Task)
import qualified TodoApp.Request as TR

newtype Opts = Opts {optCommand :: Command}

data Command
  = Add String
  | Delete Int
  | Done Int
  | View
  | ViewAll
  | Reset

main :: IO ()
main = do
  domain <- fromMaybe "http://localhost" <$> lookupEnv "TODO_DOMAIN"
  todoPort <- fromMaybe 3000 . (readMaybe =<<) <$> lookupEnv "TODO_PORT"
  (opts :: Opts) <- execParser optsParser
  case optCommand opts of
    Add task -> do
      status <- TR.add task (domain, todoPort)
      parseStatus status "Task added!"
    Delete id -> do
      status <- TR.delete id (domain, todoPort)
      parseStatus status "Task deleted!"
    Done id -> do
      status <- TR.complete id (domain, todoPort)
      parseStatus status "Task completed!"
    View -> do
      todo <- TR.view (domain, todoPort)
      printWithUtf8 todo
    ViewAll -> do
      todo <- TR.viewAll (domain, todoPort)
      printWithUtf8 todo
    Reset -> do
      status <- TR.reset (domain, todoPort)
      parseStatus status "Tasks cleared!"
  where
    printWithUtf8 :: Result [Task] -> IO ()
    printWithUtf8 res = do
      case res of
        Success a -> mapM_ printTask a
        Error b -> putStrLn b
    parseStatus :: Int -> String -> IO ()
    parseStatus status message = do
      let isStatusSuccess = status > 200 && status <= 299
          error = "Something went wrong!"
      if isStatusSuccess then putStrLn message else putStrLn error

    optsParser :: ParserInfo Opts
    optsParser =
      info
        (helper <*> versionOption <*> programOptions)
        ( fullDesc
            <> progDesc "optparse subcommands example"
            <> header
              "optparse-sub-example - a small example program for optparse-applicative with subcommands"
        )
    versionOption :: Parser (a -> a)
    versionOption = infoOption "0.0" (long "version" <> help "Show version")
    programOptions :: Parser Opts
    programOptions =
      Opts <$> hsubparser (addCommand <> deleteCommand <> doneCommand <> viewCommand <> viewAllCommand <> resetCommand)
    addCommand :: Mod CommandFields Command
    addCommand =
      command
        "add"
        (info addOptions (progDesc "Add a task"))
    addOptions :: Parser Command
    addOptions =
      Add
        <$> strArgument (metavar "Task" <> help "Task to add in TODO")
    deleteCommand :: Mod CommandFields Command
    deleteCommand =
      command
        "delete"
        (info addOptions (progDesc "Delete a task"))
    deleteOptions :: Parser Command
    deleteOptions =
      Delete
        <$> option auto (metavar "Task ID" <> help "ID of the task to delete from TODO")
    doneCommand :: Mod CommandFields Command
    doneCommand =
      command
        "done"
        (info addOptions (progDesc "Mark a task as complete"))
    doneOptions :: Parser Command
    doneOptions =
      Done
        <$> option auto (metavar "Task ID" <> help "ID of the task to mark as complete")
    viewCommand :: Mod CommandFields Command
    viewCommand =
      command
        "view"
        (info (pure View) (progDesc "View list of pending TODOs"))
    -- TODO: make this an extension for view command, for example: view -a should print
    -- all tasks
    viewAllCommand :: Mod CommandFields Command
    viewAllCommand =
      command
        "viewAll"
        (info (pure ViewAll) (progDesc "View list of all TODOs"))
    resetCommand :: Mod CommandFields Command
    resetCommand =
      command
        "reset"
        (info (pure Reset) (progDesc "Clear the TODO list"))
    -- \|Set the width of the box that displays the list of TODO's
    width :: Int
    width = 50

    getCheckBox :: Bool -> String
    getCheckBox True = "✗ "
    getCheckBox False = "✓ "

    printTask :: TR.Task -> IO ()
    printTask v = do
      printBox
        -- Move the task row to the right by 2 spaces
        ( moveRight
            2
            -- Prints `✓ <id>`
            (text (getCheckBox (TR.done v) ++ show (TR.id v)))
            <+>
            -- Prints task in a box whose width is `width` and
            -- height depends on the length of the taks message
            para left width (DT.unpack $ TR.task v)
        )
      -- Print an extra line to separate two tasks
      printBox $ text " "
