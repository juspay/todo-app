{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import TodoApp.Request (add, delete, done, reset, view)

main :: IO ()
main = do
  domain <- fromMaybe "http://localhost" <$> lookupEnv "TODO_DOMAIN"
  todoPort <- fromMaybe 3000 . (readMaybe =<<) <$> lookupEnv "TODO_PORT"
  -- Test
  view (domain, todoPort)
  reset (domain, todoPort)
  add "do bad things" (domain, todoPort)
  done 1 (domain, todoPort)
  delete 1 (domain, todoPort)
  view (domain, todoPort)

-- TODO: Use boxes to display the output of the view function
