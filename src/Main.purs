module Main
  ( main
  , handler
  , STDIN
  ) where

import Prelude

import Amazon.Alexa.Handler (Handler, makeHandler)
import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foreign (Foreign)
import Manifest (manifest)
import Model (model)
import Simple.JSON (write, writeJSON)
import Skill (handle)

usage :: String → String
usage launch = "USAGE:\n" <>
  launch <> " manifest" <> " -- print the skill manifest to stdout\n" <>
  launch <> " model" <> " -- print the language model to stdout\n" <>
  launch <> " execute" <> " -- read an AlexaRequest from stdin and execute the skill handler, printing the json response to stdout\n"

main :: forall e. Eff ( console :: CONSOLE, stdin :: STDIN | e) Unit
main = runCommand
  where
    runCommand
      | args.command == "manifest" = logPretty $ writeJSON manifest
      | args.command == "model" = logPretty $ writeJSON model
      | args.command == "execute" = handleFromStdin
      | otherwise = log $ usage (args.bin <> " " <> args.path)

    handleFromStdin  = launchAff_ do
      event <- readJsonFromStdin <#> write
      result <- map write $ handle event emptyObject
      ( liftEff <<< logPretty <<< writeJSON) result

    emptyObject :: Foreign
    emptyObject = write {}

handler :: ∀ e. Handler e
handler = makeHandler \event context → do
  result <- handle event context
  pure $ write result

foreign import logPretty :: ∀ e. String → Eff (console :: CONSOLE | e) Unit

foreign import args ::
  { bin :: String
  , path :: String
  , command :: String
  }

foreign import data STDIN :: Effect

foreign import _readJsonFromStdin :: ∀ eff. EffFnAff (stdin :: STDIN | eff) String
readJsonFromStdin :: ∀ e. Aff (stdin :: STDIN | e) String
readJsonFromStdin = fromEffFnAff _readJsonFromStdin

