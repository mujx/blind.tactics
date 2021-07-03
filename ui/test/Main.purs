module Test.Main where

import Prelude

import Blind.Data.Route (Route(..), routeCodec)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Routing.Duplex (parse)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "router" do
    it "supports the home route" $ 
      parse routeCodec "/" `shouldEqual` (Right Home)
    it "supports the problem route" $
      parse routeCodec "/problem/test" `shouldEqual` (Right $ Problem "test")

