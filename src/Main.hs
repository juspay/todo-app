module Main (main) where

import Data.Aeson (Result (..))
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import qualified Data.Text as DT
import qualified Data.Text as T
import Options.Applicative hiding (Success)
import System.Environment (lookupEnv)
import Text.PrettyPrint.Boxes hiding ((<>))
import Text.Read (readMaybe)
import Text.URI (URI, mkURI)
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
  conn <- getConnection
  -- CLI options
  opts <- execParser optsParser
  -- Run the app
  runApp conn opts

runApp :: TR.Connection -> Opts -> IO ()
runApp conn opts = do
  case optCommand opts of
    Add task -> do
      TR.runRequest conn (TR.Add task)
      putStrLn "Task added!"
    Delete id -> do
      TR.runRequest conn (TR.Delete id)
      putStrLn "Task deleted!"
    Done id -> do
      TR.runRequest conn (TR.Complete id)
      putStrLn "Task completed!"
    View -> do
      todo <- TR.runRequest conn TR.View
      mapM_ printTask todo
    ViewAll -> do
      todo <- TR.runRequest conn TR.ViewAll
      mapM_ printTask todo
    Reset -> do
      TR.runRequest conn TR.Reset
      putStrLn "Tasks cleared!"
  where
    printTask :: TR.Task -> IO ()
    printTask v = do
      printBox
        -- Move the task row to the right by 2 spaces
        ( moveRight
            2
            -- Prints `✓ <id>`
            (text (getStatusIcon (TR.done v) $ show (TR.id v)))
            <+>
            -- Prints task in a box whose width is `width` and
            -- height depends on the length of the taks message
            para left width (DT.unpack $ TR.task v)
        )
      -- Print an extra line to separate two tasks
      printBox $ text " "

    -- \|Set the width of the box that displays the list of TODO's
    width :: Int
    width = 50

    getStatusIcon :: Bool -> String -> String
    getStatusIcon True _ = "[x] "
    getStatusIcon False id = "[" ++ id ++ "] "

-- | Get `TR.Connection` to connect to the postgrest service
getConnection :: IO TR.Connection
getConnection = do
  unixSocketPath <- lookupEnv "PGRST_SERVER_UNIX_SOCKET"
  case unixSocketPath of
    Just path -> pure $ TR.UnixSocket path
    Nothing -> do
      todoUri <- lookupEnv "TODO_URI"
      postgrestHost <- lookupEnv "PGRST_SERVER_HOST"
      postgrestPort <- lookupEnv "PGRST_SERVER_PORT"
      let defaultUri = "http://" <> fromMaybe "localhost" postgrestHost <> ":" <> fromMaybe "3000" postgrestPort
      let uri = fromMaybe defaultUri todoUri
      TR.TCP <$> mkURI (T.pack uri)

optsParser :: ParserInfo Opts
optsParser =
  info
    (helper <*> versionOption <*> programOptions)
    ( fullDesc
        <> header
          "todo-app - A demo Haskell app showing the use of `flake-parts` to enable various dev workflows"
    )
  where
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
        (info deleteOptions (progDesc "Delete a task"))
    deleteOptions :: Parser Command
    deleteOptions =
      Delete
        <$> argument auto (metavar "Task_ID" <> help "ID of the task to delete from TODO")
    doneCommand :: Mod CommandFields Command
    doneCommand =
      command
        "done"
        (info doneOptions (progDesc "Mark a task as complete"))
    doneOptions :: Parser Command
    doneOptions =
      Done
        <$> argument auto (metavar "Task_ID" <> help "ID of the task to mark as complete")
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
