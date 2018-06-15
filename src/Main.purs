module Main
  ( main
  , handler
  ) where

import Prelude

import Amazon.Alexa.Handler (Handler, makeHandler)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign (Foreign)
import Manifest (manifest)
import Model (model)
import Simple.JSON (write, writeJSON)
import Skill (handle)

usage :: String → String
usage launch = "USAGE:\n" <>
  launch <> " manifest" <> " -- print the skill manifest to stdout\n" <>
  launch <> " model" <> " -- print the language model to stdout\n" <>
  launch <> " execute" <> " -- read an AlexaRequest from stdin and execute the skill handler, printing the json response to stdout\n"

main :: Effect Unit
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
      ( liftEffect <<< logPretty <<< writeJSON) result

    emptyObject :: Foreign
    emptyObject = write {}

handler :: Handler
handler = makeHandler \event context → do
  result <- handle event context
  pure $ write result

foreign import logPretty :: String → Effect Unit

foreign import args ::
  { bin :: String
  , path :: String
  , command :: String
  }


foreign import _readJsonFromStdin :: EffectFnAff String
readJsonFromStdin :: Aff String
readJsonFromStdin = fromEffectFnAff _readJsonFromStdin

