module Skill where

import Prelude

import Amazon.Alexa.LanguageModel (LanguageModel)
import Amazon.Alexa.Types (AlexaRequest(..), AlexaResponse, Speech(..))
import Data.Array.NonEmpty (appendArray)
import Data.Either (Either, hush)
import Data.Generic.Rep (class Generic)
import Data.Map (insert)
import Data.Maybe (Maybe(..))
import EasyAlexa (Builtin, languageModel, parseInput)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Simple.JSON (class ReadForeign, class WriteForeign, read, read', write)
import Type.Prelude (Proxy(..))

type Session = Maybe { status :: Status }
data Status = Counting | ConfirmingDecrement Int

derive instance genericStatus :: Generic Status _

instance readForeignStatus :: ReadForeign Status where
  readImpl f = read' f 

instance writeForeignStatus :: WriteForeign Status where
  writeImpl s = write s

data Input
  = Cancel
  | Help
  | Stop
  | Yes
  | No
  | SessionEnded
  | Increment { n :: Builtin "AMAZON.NUMBER" Int }
  | Decrement { n :: Builtin "AMAZON.NUMBER" Int }
  | Launch

derive instance genericInput :: Generic Input _

lm :: Either String LanguageModel
lm = languageModel
  (Proxy :: Proxy Input)
  "purescript template" 
  ( mempty
  # insert "Increment" (pure "Go up" `appendArray` ["go up by {n}"])
  # insert "Decrement" (pure "Go up" `appendArray` ["go down by {n}"])
  )

getInputs :: Foreign → Maybe { input :: Input, session :: Session }
getInputs event = do
  ar <- read event # hush
  input <- parseInput ar # hush
  session <- case ar of
    IntentRequest r → (hush <<< read) r.session.attributes
    _ → Nothing
  pure { input, session }

type Output a = 
  { speech :: String
  , reprompt :: Maybe String
  , session :: a
  }

renderOutput :: Output Session → AlexaResponse Session
renderOutput
  { speech
  , reprompt
  , session
  } = { version : "1.0"
      , sessionAttributes: session
      , response :
        { outputSpeech : (Just <<< Text) speech
        , card : Nothing
        , reprompt : reprompt <#> \s → { outputSpeech : Text s }
        , shouldEndSession : true
        }
      }

endAndSay :: String → Output Session
endAndSay speech = { session : Nothing, speech, reprompt : Nothing }
      
runSkill :: Input -> Session → Aff (Output Session)
runSkill Cancel        _ = pure $ endAndSay "Cancel"
runSkill Help          _ = pure $ endAndSay "Help"
runSkill Stop          _ = pure $ endAndSay "Stop"
runSkill Yes           _ = pure $ endAndSay "Yes"
runSkill No            _ = pure $ endAndSay "No"
runSkill SessionEnded  _ = pure $ endAndSay "SessionEnded"
runSkill (Increment _) _ = pure $ endAndSay "Increment"
runSkill (Decrement _) _ = pure $ endAndSay "Decrement"
runSkill Launch        _ = pure $ endAndSay "Start"

handle :: Foreign → Foreign → Aff (AlexaResponse Session)
handle event _ =
  renderOutput <$>
    case getInputs event of
      Nothing → pure $ endAndSay "Oops"
      Just {input, session} → runSkill input session

--   output <- case read event of
--     Left _ → runSkill (ErrorInput "Couldn't read event") Nothing
--     Right (LaunchRequest r) → runSkill Start Nothing
--     Right (SessionEndedRequest r) → runSkill End Nothing
--     Right (IntentRequest r) → runSkill (parsedIntent r) (parsedSession r.session.attributes)
--   pure 
--     { version : "1.0"
--     , response : 
--       { card : Nothing
--       , outputSpeech : Just (Text output.speech)
--       , reprompt : output.reprompt <#> \x → { outputSpeech : Text x }
--       , shouldEndSession : isNothing output.session
--       }
--     , sessionAttributes: output.session
--     }
--   where
--     parsedIntent r = readIntent r.request.intent.name r.request.intent.slots
--     parsedSession attrs = case read attrs of
--       Left _ → Nothing
--       Right sess → sess
-- 
-- runSkill :: Input → Session → Aff (Output)
-- runSkill = run
--   where
--     run (ErrorInput s) _           = handleError s
--     run _              Nothing     = welcome
--     run Start          _           = welcome
--     run Help           sess        = readInstructions sess
--     run Stop           (Just sess) = endSession sess.counter
--     
--     run (Increment n) (Just sess) = doIncrement sess n
--     run (Decrement n) (Just sess) = confirmDecrement sess n
--     
--     run Yes    (Just sess@{ status : ConfirmingDecrement n}) = doDecrement sess n
--     run Cancel (Just sess@{ status : ConfirmingDecrement _}) = cancelDecrement sess
--     run No     (Just sess@{ status : ConfirmingDecrement n}) = cancelDecrement sess
-- 
--     run Yes    (Just sess@{ status : Counting })             = didntUnderstand sess
--     run No     (Just sess@{ status : Counting })             = didntUnderstand sess
--     run Cancel (Just sess@{ status : Counting })             = endSession sess.counter
-- 
--     run End _                                                = noop
-- 
--     handleError s =
--       pure
--         { session : Nothing
--         , speech : "Something went wrong."
--         , reprompt : Nothing
--         }
--     
--     welcome =
--       pure
--         { session : Just { status : Counting, counter : 1 }
--         , speech : "Welcome to the purescript alexa template. The counter is at 1. Say increment to go up. Say decrement to go down."
--         , reprompt: Just "Say increment to go up. Say decrement to go down."
--         }
--     
--     readInstructions sess =
--       pure
--         { session : sess
--         , speech : "Say increment to go up. Say decrement to go down."
--         , reprompt : Just "Say increment to go up. Say decrement to go down."
--         }
-- 
--     endSession counter = 
--       pure
--         { session : Nothing
--         , speech : "Goodbye. The final count was " <> (show counter)
--         , reprompt : Nothing
--         }
-- 
--     doIncrement sess n =
--       pure
--         { session : Just sess { counter = new }
--         , speech : "Ok. I incremented the counter by " <> (show n) <> ". The counter is now at " <> (show new) <> ". What next?"
--         , reprompt : Just "Say increment to go up. Say decrement to go down."
--         }
--         where
--           new = sess.counter + n
--     
--     doDecrement sess n =
--       pure
--         { session : Just sess { counter = new }
--         , speech : "Ok. I decremented the counter by " <> (show n) <> ". The counter is now at " <> (show new) <> ". What next?"
--         , reprompt : Just "Say increment to go up. Say decrement to go down."
--         }
--         where
--           new = sess.counter - n
-- 
--     confirmDecrement sess n =
--       pure
--         { session : Just sess { status = ConfirmingDecrement n }
--         , speech : "Decrement the counter by " <> (show n) <> "? That seems pretty drastic. Are you sure?"
--         , reprompt : Just $ "Are you sure you want to decrement the counter by " <> (show n) <> "? Say yes or no."
--         }
--     
--     cancelDecrement sess =
--       pure
--         { session : Just sess
--         , speech : "The decrement operation has been cancelled. The counter is at " <> (show sess.counter) <> ". Say increment to go up. Say decrement to go down."
--         , reprompt: Just $ "The counter is at " <> (show sess.counter) <> ". Say increment to go up. Say decrement to go down."
--         }
--     
--     didntUnderstand sess =
--       pure
--         { session : Just sess
--         , speech : "Sorry, I didn't understand."
--         , reprompt : Just "Say increment to go up. Say decrement to go down."
--         }
--     
--     noop = pure { session : Nothing, speech: "", reprompt : Nothing }
