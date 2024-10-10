{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module TodoApp.Request
  ( -- * Task type
    Task (..),

    -- * Interacting with tasks
    Request (..),
    runRequest,
  )
where

import Control.Lens ((&), (.~))
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
import Text.URI (QueryParam (..), URI, mkQueryKey, mkQueryValue, mkURI)
import Text.URI.Lens (queryParam, uriQuery)
import Prelude hiding (concat)

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

runRequest ::
  (MonadIO m, m ~ IO) =>
  Request a ->
  (String, Int) ->
  m a
runRequest = \case
  Complete id -> complete id
  ViewAll -> viewAll
  View -> view
  Add task -> add task
  Delete id -> delete id
  Reset -> reset

-- | Mark the item with id as done
complete :: Int -> (String, Int) -> IO ()
complete id host = do
  let payload =
        object ["done" .= ("true" :: String)]
  q <- QueryParam <$> mkQueryKey "id" <*> mkQueryValue (pack $ show id)
  void $ request (R.ReqBodyJson payload) R.PATCH "/todos" [q] host

-- | Return all the items in the table
viewAll :: (String, Int) -> IO [Task]
viewAll host = do
  res <- request R.NoReqBody R.GET "/todos" [] host
  let v = fromJSON $ fromMaybe (object []) $ decode res
  pure $ case v of
    Aeson.Success a -> a
    Aeson.Error e -> error e

-- | Return pending items in the table
view :: (String, Int) -> IO [Task]
view host = do
  q <- QueryParam <$> mkQueryKey "done" <*> mkQueryValue "is.false"
  res <- request R.NoReqBody R.GET "/todos" [q] host
  let v = fromJSON $ fromMaybe (object []) $ decode res
  pure $ case v of
    Aeson.Success a -> a
    Aeson.Error e -> error e

-- | Add a new task to the table
add :: String -> (String, Int) -> IO ()
add task host = do
  let payload =
        object ["task" .= task]
  void $ request (R.ReqBodyJson payload) R.POST "/todos" [] host

-- | Delete a TODO item with given id
delete :: Int -> (String, Int) -> IO ()
delete id host = do
  q <- QueryParam <$> mkQueryKey "id" <*> mkQueryValue (pack $ show id)
  void $ request R.NoReqBody R.DELETE "/todos" [q] host

-- | Remove all the TODO items from the table
reset :: (String, Int) -> IO ()
reset host = do
  _ <- request R.NoReqBody R.DELETE "/todos" [] host
  -- Call a SQL function that sets the sequence to start from 1
  void $ request R.NoReqBody R.POST "/rpc/reset_id" [] host

data Response = Response
  { message :: ByteString,
    responseCode :: Int
  }

-- TODO: Add more comments

-- | Make a http request to postgrest service
request ::
  ( R.HttpBodyAllowed (R.AllowsBody method) (R.ProvidesBody body),
    MonadIO m,
    R.HttpMethod method,
    R.HttpBody body
  ) =>
  body ->
  method ->
  Text ->
  [QueryParam] ->
  (String, Int) ->
  m ByteString
request body method subdir qs (domain, todoPort) =
  R.runReq R.defaultHttpConfig $ do
    uri <- mkURI $ concat [pack domain, subdir]
    let (url, options) = fromJust $ R.useHttpURI $ uri & uriQuery .~ qs
    r <-
      R.req
        method
        url
        body
        R.lbsResponse
        $ R.port todoPort <> options -- options include the query parameters that help in filtering rows of a table
    let responseCode = R.responseStatusCode r :: Int
    if responseCode >= 200 && responseCode < 300
      then return $ R.responseBody r
      else error "Request failed"
