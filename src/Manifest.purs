module Manifest where

import Prelude

import Amazon.Alexa.Manifest (Manifest)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Maybe (Maybe(..))
import Data.StrMap (empty, singleton)

manifest :: Manifest
manifest =
  { skillManifest :
      { publishingInformation :
          { locales :
              singleton "en-US"
                { name : "purescript template"
                , summary : "template skill"
                , examplePhrases :
                    [ "Alexa, open purescript template"
                    , "Increment the counter"
                    , "Decrement the counter"
                    ]
                  , description : "This is a skill template for you to clone if you want to easily get started writing an Alexa Skill in purescript. It implements a simple counter that can be incremented and decremented."
                , keywords :
                    [ "purescript"
                    , "skill"
                    , "template"
                    ]
                }
          , isAvailableWorldWide : true
          , testingInstructions : ""
          , category : "GAMES"
          , distributionCountries : []
          }
      , apis : 
          { "custom" :
              { "endpoint":
                  { "sourceDir" : NullOrUndefined $ Just "output"
                  , "uri" : NullOrUndefined $ Nothing
                  }
              }
          }
      , manifestVersion : "1.0"
      , permissions : []
      , privacyAndCompliance :
          { allowsPurchases : false
          , usesPersonalInfo : false
          , isChildDirected : false
          , isExportCompliant : true
          , containsAds : false
          , locales : empty
          }
      , events : NullOrUndefined Nothing
      , subscriptions : []
      , regions : empty
      }
  }
