module Skill where

import Prelude

import Amazon.Alexa.Types (AlexaRequest(..), AlexaResponse, Speech(..))
import Control.Monad.Aff (Aff)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign (Foreign, ForeignError(..), fail)
import Data.Maybe (Maybe(..), isNothing)
import Simple.JSON (class ReadForeign, class WriteForeign, read, write)

type Session = Maybe { counter :: Int, status :: Status }
data Status = Counting | ConfirmingDecrement

instance wfStatus :: WriteForeign Status where
  writeImpl Counting = write "Counting"
  writeImpl ConfirmingDecrement = write "ConfirmingDecrement"
instance rfStatus :: ReadForeign Status where
  readImpl f = read f >>= match
    where
      match x
        | x == "ConfirmingDecrement" = pure ConfirmingDecrement
        | otherwise = (fail <<< ForeignError) ("Unknown status: " <> x)

data Input
  = Cancel
  | Help
  | Stop
  | Yes
  | No
  | Increment (Maybe Int)
  | Decrement (Maybe Int)
  | ErrorInput

type Output =
  { speech :: String
  , reprompt :: Maybe String
  , session :: Session
  }

handle :: ∀ e. Foreign → Foreign → Aff e (AlexaResponse Session)
handle event _ = do
  output <- case runExcept (read event) of
    Left _ → runSkill ErrorInput Nothing
    Right (LaunchRequest r) → runSkill ErrorInput Nothing
    Right (IntentRequest r) → runSkill ErrorInput Nothing
    Right (SessionEndedRequest r) → runSkill ErrorInput Nothing
  pure 
    { version : "1.0"
    , response : 
      { card : Nothing
      , outputSpeech : Just (Text output.speech)
      , reprompt : output.reprompt <#> \x → { outputSpeech : Text x }
      , shouldEndSession : isNothing output.session
      }
    , sessionAttributes: output.session
    }

runSkill :: ∀ e. Input → Session → Aff e (Output)
runSkill (ErrorInput) _ =
  pure
    { session : Nothing
    , speech : "Something went wrong."
    , reprompt : Nothing
    }

runSkill _ Nothing = 
  pure
    { session : Just { status : Counting, counter : 1 }
    , speech : "Welcome to the purescript alexa template. The counter is at 1. Say increment to go up. Say decrement to go down."
    , reprompt: Just "Say increment to go up. Say decrement to go down."
    }

runSkill Stop (Just sess@{ counter : counter }) = 
  pure
    { session : Just sess
    , speech : "Goodbye. The final count was " <> (show counter)
    , reprompt : Nothing
    }

runSkill Cancel (Just sess@{ status : ConfirmingDecrement, counter : counter }) = 
  pure
    { session : Just sess
    , speech : "The decrement operation has been cancelled. The counter is at " <> (show counter) <> ". Say increment to go up. Say decrement to go down."
    , reprompt: Just $ "The counter is at " <> (show counter) <> ". Say increment to go up. Say decrement to go down."
    }

runSkill Cancel (Just sess@{ status : _, counter : counter }) = 
  pure
    { session : Just sess
    , speech : "Goodbye. The final count was " <> (show counter)
    , reprompt : Nothing
    }

runSkill _ _ =
  pure
    { session : Nothing
    , speech : "TODO"
    , reprompt: Nothing
    }
