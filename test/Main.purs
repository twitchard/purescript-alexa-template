module Test.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Simple.JSON (read, write)
import Skill (Status(..))
import Test.Unit (failure, success, suite, test)
import Test.Unit.Assert (equal) as Assert
import Test.Unit.Main (runTest)



main :: Effect Unit
main = runTest do
  suite "Status" do
     test "encodeCounting" do
        let result = read <<< write $ Counting
        case result of
          Right Counting → success
          Right _ → failure "Counting was decoded into the wrong Status"
          Left _ → failure "Counting could not be decoded into a Status"
     test "encodeConfirmingDecrement" do
        let result = read <<< write $ ConfirmingDecrement 4
        case result of
          Right (ConfirmingDecrement x) → Assert.equal x 4
          Right _ → failure "ConfirmingDecrement was decoded into the wrong Status"
          Left _ → failure "ConfirmingDecrement could not be decoded into a Status"
