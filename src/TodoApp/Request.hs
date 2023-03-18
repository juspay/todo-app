{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module TodoApp.Request (done, view, add, delete, reset) where

-- TODO: maybe use qualified import?

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (ToJSON, Value)
import Data.Kind (Type)
import Data.Maybe (fromJust)
import Data.Text (concat, pack)
import GHC.IO (liftIO)
import Network.HTTP.Req
  ( AllowsBody,
    CanHaveBody (CanHaveBody),
    HttpBodyAllowed,
    HttpMethod,
    ReqBodyJson (..),
    defaultHttpConfig,
    ignoreResponse,
    jsonResponse,
    port,
    req,
    responseBody,
    responseStatusCode,
    runReq,
    useHttpURI,
  )
import Relude (fromMaybe, lookupEnv, readMaybe, toText)
import Text.URI (mkURI)
import Prelude hiding (concat)

data Response = Response
  { message :: Value,
    responseCode :: Int
  }

request ::
  forall {method} {m :: Type -> Type} {a}.
  ( HttpBodyAllowed (AllowsBody method) 'CanHaveBody,
    MonadIO m,
    HttpMethod method,
    ToJSON a
  ) =>
  a ->
  method ->
  m Response
request payload method =
  runReq defaultHttpConfig $ do
    -- TODO: Move domain and port to main and use IORef to Store the state
    domain <- fromMaybe "http://localhost" <$> lookupEnv "TODO_DOMAIN"
    todoPort <- (fromMaybe 3000 <$> readMaybe) . fromMaybe "3000" <$> lookupEnv "TODO_PORT"
    uri <- mkURI $ concat [toText domain, "/todos"]
    let (url, _) = fromJust (useHttpURI uri)
    r <-
      req
        method
        url
        (ReqBodyJson payload) -- use built-in options or add your own
        jsonResponse
        (port todoPort) -- query params, headers, explicit port number, etc.
    return Response {message = responseBody r, responseCode = responseStatusCode r :: Int}

done = undefined

view = undefined

add = undefined

delete = undefined

reset = undefined
