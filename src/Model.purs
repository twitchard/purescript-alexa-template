module Model where

import Amazon.Alexa.LanguageModel (LanguageModel)

model ::
  { interactionModel ::
    { languageModel :: LanguageModel }
  }
model =
  { interactionModel :
    { languageModel : americanEnglish }
  }

americanEnglish :: LanguageModel
americanEnglish =
  { invocationName: "purescript template"
  , intents:
      [ { name: "AMAZON.CancelIntent" , samples: [] , slots : [] }
      , { name: "AMAZON.HelpIntent" , samples: [] , slots : [] }
      , { name: "AMAZON.StopIntent" , samples: [] , slots : [] }
      , { name: "AMAZON.YesIntent" , samples: [] , slots : [] }
      , { name: "AMAZON.NoIntent" , samples: [] , slots : [] }
      , { name: "IncrementIntent"
        , samples: [ "Increment the counter"
                   , "Increment it"
                   , "Increment it by {Num}"
                   , "Increment"
                   , "Go up"
                   , "Go up by {Num}"
                   , "Add {Num}"
                   , "Plus {Num}"
                   ]
        , slots : [ { "name": "Num"
                    , "type": "AMAZON.NUMBER"
                    }
                  ]
        }
      , { name: "DecrementIntent"
        , samples: [ "Decrement the counter"
                   , "Decrement it"
                   , "Decrement"
                   , "Go down"
                   , "Go down by {Num}"
                   , "Subtract {Num}"
                   , "Minus {Num}"
                   ]
        , slots : [ { "name": "Num"
                    , "type": "AMAZON.NUMBER"
                    }
                  ]
        }
      ]
  , types: [ ]
  }
