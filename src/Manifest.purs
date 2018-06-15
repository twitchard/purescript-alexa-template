module Manifest where

import Prelude

import Amazon.Alexa.Manifest (Manifest)
import Data.Maybe (Maybe(..))
import Foreign.Object (empty, singleton)

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
          , testingInstructions : ""
          , category : "GAMES"
          , distributionCountries : [ "US" ]
          }
      , apis : 
          { "custom" :
              { "endpoint":
                 { "sourceDir" : Just "output"
                  , "uri" : Nothing
                  }
              , "interfaces" : Nothing
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
      , events : Nothing
      , subscriptions : Nothing
      , regions : Nothing
      }
  }
