{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module TodoApp.Request (done, view, add, delete, reset) where
-- TODO: maybe use qualified import?
import Network.HTTP.Req
    ( responseStatusCode,
      port,
      ignoreResponse,
      req,
      useHttpURI,
      defaultHttpConfig,
      runReq,
      ReqBodyJson(..),
      HttpMethod, jsonResponse, responseBody, HttpBodyAllowed, AllowsBody, CanHaveBody (CanHaveBody))
import Text.URI (mkURI)
import Data.Maybe (fromJust)
import Data.Text (concat, pack)
import Prelude hiding (concat)
import Data.Aeson (Value, ToJSON)
import Data.Kind (Type)
import Control.Monad.IO.Class (MonadIO)
import GHC.IO (liftIO)
import Relude (lookupEnv, toText, fromMaybe, readMaybe)

data Response = Response {
   message :: Value
  ,responseCode :: Int
}
request :: forall {method} {m :: Type -> Type} {a}.
  (HttpBodyAllowed (AllowsBody method) 'CanHaveBody,
   MonadIO m,
   HttpMethod method,
   ToJSON a) =>
   a -> method -> m Response
request payload method = 
  runReq defaultHttpConfig $ do
    -- TODO: Move domain and port to main and use IORef to Store the state 
    domain <- fromMaybe "http://localhost" <$> lookupEnv "TODO_DOMAIN"
    todoPort <- ( fromMaybe 3000 <$> readMaybe ) . fromMaybe "3000" <$> lookupEnv "TODO_PORT"
    uri <- mkURI $ concat [toText domain, "/todos"]
    let (url, _) = fromJust (useHttpURI uri)
    r <-
      req
        method
        url
        (ReqBodyJson payload) -- use built-in options or add your own
        jsonResponse
        (port todoPort) -- query params, headers, explicit port number, etc.
    return Response { message = responseBody r ,responseCode = responseStatusCode r :: Int }

done = undefined

view = undefined

add = undefined

delete = undefined

reset = undefined

