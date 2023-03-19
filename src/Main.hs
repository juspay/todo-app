{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Main.Utf8 as Utf8
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import TodoApp.Request (add, delete, complete, reset, view, viewPending)
import Text.PrettyPrint.Boxes
    ( (//), (<+>), para, printBox, right, text )
import Data.Aeson (Result(..))

main :: IO ()
main = do
  domain <- fromMaybe "http://localhost" <$> lookupEnv "TODO_DOMAIN"
  todoPort <- fromMaybe 3000 . (readMaybe =<<) <$> lookupEnv "TODO_PORT"
  -- Test
  view (domain, todoPort)
  reset (domain, todoPort)
  add "do bad things" (domain, todoPort)
  add "do bad things" (domain, todoPort)
  complete 1 (domain, todoPort)
  delete 1 (domain, todoPort)
  res <- view (domain, todoPort)
  case res of
    Success a -> print a
    Error b -> print b
  print <$> viewPending (domain, todoPort)
  Utf8.withUtf8 $ do
    printBox (( text "✗" <+> para right 100 "Hello 🌎" ) // ( text "✓" <+> para right 100 "Hello 🌎"))

-- TODO: Use boxes to display the output of the view function
