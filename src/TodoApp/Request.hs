{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module TodoApp.Request
  ( -- * Task type
    Task (..),

    -- * Interacting with tasks
    Request (..),
    runRequest,
    Connection (..),
  )
where

import Control.Lens ((&), (.~), (?~))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (Result, decode, fromJSON, object, (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (FromJSON)
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Kind (Type)
import Data.Maybe (fromJust, fromMaybe)
import Data.String (IsString (fromString))
import Data.Text (Text, concat, pack)
import GHC.Generics (Generic)
import qualified Network.HTTP.Req as R
import Text.URI (Authority (Authority), QueryParam (..), RText, RTextLabel (Host, PathPiece), URI, emptyURI, mkHost, mkPathPiece, mkQueryKey, mkQueryValue, mkScheme, mkURI)
import Text.URI.Lens (queryParam, uriAuthority, uriPath, uriQuery, uriScheme)
import Prelude hiding (concat)
import qualified Network.HTTP.Client as HC
import qualified Network.Socket as NS

type TaskId = Int

data Task = Task
  { id :: TaskId,
    done :: Bool,
    task :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON)

data Request r where
  -- | Mark a task as complete
  Complete :: TaskId -> Request ()
  -- | Return all tasks
  ViewAll :: Request [Task]
  -- | Return pending tasks
  View :: Request [Task]
  -- | Add a new task
  Add :: String -> Request ()
  -- | Delete a task
  Delete :: TaskId -> Request ()
  -- | Remove all tasks
  Reset :: Request ()

data Connection
  = TCP URI
  | UnixSocket FilePath

createUnixSocketManager :: FilePath -> IO HC.Manager
createUnixSocketManager socketPath = HC.newManager $ HC.defaultManagerSettings
  { HC.managerRawConnection = return $ \_ _ _ -> do
      sock <- NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol
      NS.connect sock (NS.SockAddrUnix socketPath)
      HC.socketConnection sock 8192
  }

runRequest ::
  (MonadIO m, m ~ IO) =>
  Request a ->
  Connection ->
  m a
runRequest req conn = case req of
  Complete id -> complete id conn
  ViewAll -> viewAll conn
  View -> view conn
  Add task -> add task conn
  Delete id -> delete id conn
  Reset -> reset conn

-- | Mark the item with id as done
complete :: Int -> Connection -> IO ()
complete id conn = do
  let payload =
        object ["done" .= ("true" :: String)]
  q <- QueryParam <$> mkQueryKey "id" <*> mkQueryValue ("eq." <> pack (show id))
  path <- traverse mkPathPiece ["todos"]
  void $ request (R.ReqBodyJson payload) R.PATCH path [q] conn

-- | Return all the items in the table
viewAll :: Connection -> IO [Task]
viewAll conn = do
  path <- traverse mkPathPiece ["todos"]
  res <- request R.NoReqBody R.GET path [] conn
  let v = fromJSON $ fromMaybe (object []) $ decode res
  pure $ case v of
    Aeson.Success a -> a
    Aeson.Error e -> error e

-- | Return pending items in the table
view :: Connection -> IO [Task]
view conn = do
  path <- traverse mkPathPiece ["todos"]
  q <- QueryParam <$> mkQueryKey "done" <*> mkQueryValue "is.false"
  res <- request R.NoReqBody R.GET path [q] conn
  let v = fromJSON $ fromMaybe (object []) $ decode res
  pure $ case v of
    Aeson.Success a -> a
    Aeson.Error e -> error e

-- | Add a new task to the table
add :: String -> Connection -> IO ()
add task conn = do
  let payload =
        object ["task" .= task]
  path <- traverse mkPathPiece ["todos"]
  void $ request (R.ReqBodyJson payload) R.POST path [] conn

-- | Delete a TODO item with given id
delete :: Int -> Connection -> IO ()
delete id conn = do
  path <- traverse mkPathPiece ["todos"]
  q <- QueryParam <$> mkQueryKey "id" <*> mkQueryValue ("eq." <> pack (show id))
  void $ request R.NoReqBody R.DELETE path [q] conn

-- | Remove all the TODO items from the table
reset :: Connection -> IO ()
reset conn = do
  path <- traverse mkPathPiece ["todos"]
  _ <- request R.NoReqBody R.DELETE path [] conn
  -- Call a SQL function that sets the sequence to start from 1
  path <- traverse mkPathPiece ["rpc", "reset_id"]
  void $ request R.NoReqBody R.POST path [] conn

-- TODO: Add more comments

-- | Make a http request to postgrest service
request ::
  ( R.HttpBodyAllowed (R.AllowsBody method) (R.ProvidesBody body),
    R.HttpMethod method,
    R.HttpBody body
  ) =>
  body ->
  method ->
  [RText 'PathPiece] ->
  [QueryParam] ->
  Connection ->
  IO ByteString
request body method paths qs conn = do
  manager <- case conn of
        TCP _ -> pure R.defaultHttpConfig
        UnixSocket socketPath -> do
          socketManager <- createUnixSocketManager socketPath
          pure $ R.defaultHttpConfig { R.httpConfigAltManager = Just socketManager }
  R.runReq manager $ do
    let uri = case conn of
          TCP u -> u
          -- Even though the connection is over unix socket, `req` expects a URL-like string,
          -- so we use a dummy hostname.
          UnixSocket _ -> fromJust $ mkURI "http://localhost"

    let uri' =
          uri
            & uriQuery .~ qs
            & uriPath .~ paths
    let (url, options) = fromJust $ R.useHttpURI uri'
    r <-
      R.req
        method
        url
        body
        R.lbsResponse
        options -- options include the query parameters that help in filtering rows of a table
    let responseCode = R.responseStatusCode r :: Int
    if responseCode >= 200 && responseCode < 300
      then return $ R.responseBody r
      else error "Request failed"

