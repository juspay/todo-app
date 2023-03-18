{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module TodoApp.Request (done, view, add, delete, reset) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (object, (.=))
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Maybe (fromJust)
import Data.Text (Text, concat, pack)
import qualified Network.HTTP.Req as R
import Text.URI (mkURI)
import Prelude hiding (concat)

data Response = Response
  { message :: ByteString,
    responseCode :: Int
  }
-- TODO: Add more comments
-- |Make a http request to postgrest service
request ::
  forall {method} {m :: Type -> Type} {body} {response}.
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
        R.bsResponse
        $ R.port todoPort <> options -- options include the query parameters that help in filtering rows of a table
    return Response {message = R.responseBody r, responseCode = R.responseStatusCode r :: Int}

-- |Mark the item with id as done
done :: Int -> (String, Int) -> IO ()
done id host = do
  let payload =
        object ["done" .= ("false" :: String)]
  res <- request (R.ReqBodyJson payload) R.PATCH "/todos" (concat ["id=eq.", pack $ show id]) host
  print $ responseCode res

-- TODO: add another function that only displays pending tasks
-- |Display all the items in the table
view :: (String, Int) -> IO ()
view host = do
  res <- request R.NoReqBody R.GET "/todos" "" host
  print $ message res

-- |Add a new task to the table
add :: String -> (String, Int) -> IO ()
add task host = do
  let payload =
        object ["task" .= task]
  res <- request (R.ReqBodyJson payload) R.POST "/todos" "" host
  print $ responseCode res

-- |Delete a TODO item with given id
delete :: Int -> (String, Int) -> IO ()
delete id host = do
  res <- request R.NoReqBody R.DELETE "/todos" (concat ["id=eq.", pack $ show id]) host
  print $ responseCode res

-- |Remove all the TODO items from the table
reset :: (String, Int) -> IO ()
reset host = do
  _ <- request R.NoReqBody R.DELETE "/todos" "" host
  -- Call a SQL function that sets the sequence to start from 1
  res <- request R.NoReqBody R.POST "/rpc/reset_id" "" host
  print $ responseCode res
