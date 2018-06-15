module Skill where

import Prelude

import Amazon.Alexa.Types (AlexaRequest(..), AlexaResponse, BuiltInIntent(..), Speech(..), readBuiltInIntent)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Effect.Aff (Aff)
import Foreign (Foreign, ForeignError(..), fail)
import Simple.JSON (class ReadForeign, class WriteForeign, read, read', write)

type Session = Maybe { counter :: Int, status :: Status }
data Status = Counting | ConfirmingDecrement Int

instance wfStatus :: WriteForeign Status where
  writeImpl Counting = write { name : "Counting" }
  writeImpl (ConfirmingDecrement n) = write { name : "ConfirmingDecrement", n : n }
instance rfStatus :: ReadForeign Status where
  readImpl f = read' f >>= match
    where
      match (status :: { name :: String, n :: Maybe Int } )
        | status.name == "ConfirmingDecrement" =
            maybe
              ((fail <<< ForeignError) ("ConfirmingDecrement status without number."))
              (\x → pure $ ConfirmingDecrement x) 
              status.n
        | status.name == "Counting" = pure Counting
        | otherwise = (fail <<< ForeignError) ("Unknown status: " <> status.name)

data Input
  = Cancel
  | Help
  | Stop
  | Yes
  | No
  | End
  | Increment Int
  | Decrement Int
  | Start
  | ErrorInput String

type Output =
  { speech :: String
  , reprompt :: Maybe String
  , session :: Session
  }

readIntent :: String → Foreign → Input
readIntent intentName slots =
  case readBuiltInIntent intentName of
    Just AmazonYesIntent → Yes
    Just AmazonNoIntent → No
    Just AmazonHelpIntent → Help
    Just AmazonStopIntent → Stop
    Just AmazonCancelIntent → Cancel
    Just _ → ErrorInput $ "Unsupported built-in intent: " <> intentName
    Nothing → readCustomIntent
    where
      readCustomIntent
        | intentName == "IncrementIntent" = Increment number
        | intentName == "DecrementIntent" = Decrement number
        | otherwise = ErrorInput $ "Unrecognized intent: " <> intentName
      number = case read slots of
        Right (r :: {"Num" :: { value :: String } }) → case fromString r."Num".value of
          Just n → n
          Nothing → 1
        Left _ → 1

handle :: Foreign → Foreign → Aff (AlexaResponse Session)
handle event _ = do
  output <- case read event of
    Left _ → runSkill (ErrorInput "Couldn't read event") Nothing
    Right (LaunchRequest r) → runSkill Start Nothing
    Right (SessionEndedRequest r) → runSkill End Nothing
    Right (IntentRequest r) → runSkill (parsedIntent r) (parsedSession r.session.attributes)
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
  where
    parsedIntent r = readIntent r.request.intent.name r.request.intent.slots
    parsedSession attrs = case read attrs of
      Left _ → Nothing
      Right sess → sess

runSkill :: Input → Session → Aff (Output)
runSkill = run
  where
    run (ErrorInput s) _           = handleError s
    run _              Nothing     = welcome
    run Start          _           = welcome
    run Help           sess        = readInstructions sess
    run Stop           (Just sess) = endSession sess.counter
    
    run (Increment n) (Just sess) = doIncrement sess n
    run (Decrement n) (Just sess) = confirmDecrement sess n
    
    run Yes    (Just sess@{ status : ConfirmingDecrement n}) = doDecrement sess n
    run Cancel (Just sess@{ status : ConfirmingDecrement _}) = cancelDecrement sess
    run No     (Just sess@{ status : ConfirmingDecrement n}) = cancelDecrement sess

    run Yes    (Just sess@{ status : Counting })             = didntUnderstand sess
    run No     (Just sess@{ status : Counting })             = didntUnderstand sess
    run Cancel (Just sess@{ status : Counting })             = endSession sess.counter

    run End _                                                = noop

    handleError s =
      pure
        { session : Nothing
        , speech : "Something went wrong."
        , reprompt : Nothing
        }
    
    welcome =
      pure
        { session : Just { status : Counting, counter : 1 }
        , speech : "Welcome to the purescript alexa template. The counter is at 1. Say increment to go up. Say decrement to go down."
        , reprompt: Just "Say increment to go up. Say decrement to go down."
        }
    
    readInstructions sess =
      pure
        { session : sess
        , speech : "Say increment to go up. Say decrement to go down."
        , reprompt : Just "Say increment to go up. Say decrement to go down."
        }

    endSession counter = 
      pure
        { session : Nothing
        , speech : "Goodbye. The final count was " <> (show counter)
        , reprompt : Nothing
        }

    doIncrement sess n =
      pure
        { session : Just sess { counter = new }
        , speech : "Ok. I incremented the counter by " <> (show n) <> ". The counter is now at " <> (show new) <> ". What next?"
        , reprompt : Just "Say increment to go up. Say decrement to go down."
        }
        where
          new = sess.counter + n
    
    doDecrement sess n =
      pure
        { session : Just sess { counter = new }
        , speech : "Ok. I decremented the counter by " <> (show n) <> ". The counter is now at " <> (show new) <> ". What next?"
        , reprompt : Just "Say increment to go up. Say decrement to go down."
        }
        where
          new = sess.counter - n

    confirmDecrement sess n =
      pure
        { session : Just sess { status = ConfirmingDecrement n }
        , speech : "Decrement the counter by " <> (show n) <> "? That seems pretty drastic. Are you sure?"
        , reprompt : Just $ "Are you sure you want to decrement the counter by " <> (show n) <> "? Say yes or no."
        }
    
    cancelDecrement sess =
      pure
        { session : Just sess
        , speech : "The decrement operation has been cancelled. The counter is at " <> (show sess.counter) <> ". Say increment to go up. Say decrement to go down."
        , reprompt: Just $ "The counter is at " <> (show sess.counter) <> ". Say increment to go up. Say decrement to go down."
        }
    
    didntUnderstand sess =
      pure
        { session : Just sess
        , speech : "Sorry, I didn't understand."
        , reprompt : Just "Say increment to go up. Say decrement to go down."
        }
    
    noop = pure { session : Nothing, speech: "", reprompt : Nothing }
