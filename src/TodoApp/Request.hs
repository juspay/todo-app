module TodoApp.Request (complete, viewAll, add, delete, reset, view, Task (..)) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (Result, decode, fromJSON, object, (.=))
import Data.Aeson.Types (FromJSON)
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Kind (Type)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text, concat, pack)
import GHC.Generics (Generic)
import qualified Network.HTTP.Req as R
import Text.URI (mkURI)
import Prelude hiding (concat)

data Response = Response
  { message :: ByteString,
    responseCode :: Int
  }

data Task = Task
  { id :: Int,
    done :: Bool,
    task :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON)

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
  Text ->
  (String, Int) ->
  m Response
request body method subdir filter (domain, todoPort) =
  R.runReq R.defaultHttpConfig $ do
    uri <- mkURI $ concat [pack domain, subdir, "?", filter]
    let (url, options) = fromJust (R.useHttpURI uri)
    r <-
      R.req
        method
        url
        body
        R.lbsResponse
        $ R.port todoPort <> options -- options include the query parameters that help in filtering rows of a table
    return Response {message = R.responseBody r, responseCode = R.responseStatusCode r :: Int}

-- | Mark the item with id as done
complete :: Int -> (String, Int) -> IO Int
complete id host = do
  let payload =
        object ["done" .= ("true" :: String)]
  res <- request (R.ReqBodyJson payload) R.PATCH "/todos" (concat ["id=eq.", pack $ show id]) host
  return $ responseCode res

-- | Return all the items in the table
viewAll :: (String, Int) -> IO (Result [Task])
viewAll host = do
  res <- request R.NoReqBody R.GET "/todos" "" host
  return $ fromJSON $ fromMaybe (object []) $ decode $ message res

-- | Return pending items in the table
view :: (String, Int) -> IO (Result [Task])
view host = do
  res <- request R.NoReqBody R.GET "/todos" "done=is.false" host
  return $ fromJSON $ fromMaybe (object []) $ decode $ message res

-- | Add a new task to the table
add :: String -> (String, Int) -> IO Int
add task host = do
  let payload =
        object ["task" .= task]
  res <- request (R.ReqBodyJson payload) R.POST "/todos" "" host
  return $ responseCode res

-- | Delete a TODO item with given id
delete :: Int -> (String, Int) -> IO Int
delete id host = do
  res <- request R.NoReqBody R.DELETE "/todos" (concat ["id=eq.", pack $ show id]) host
  return $ responseCode res

-- | Remove all the TODO items from the table
reset :: (String, Int) -> IO Int
reset host = do
  _ <- request R.NoReqBody R.DELETE "/todos" "" host
  -- Call a SQL function that sets the sequence to start from 1
  res <- request R.NoReqBody R.POST "/rpc/reset_id" "" host
  return $ responseCode res
