module Main where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Data.Maybe (Maybe(..))
import Routing.Hash (matchesWith)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Blind.Data.Route (routeCodec)
import Routing.Duplex (parse)
import Halogen as H
import Blind.Component.Router as Router
import Blind.AppM (runAppM)
import Blind.Env (Env, LogLevel(..))
import Blind.Api.Request (BaseURL(..))

main :: Effect Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    let
      environment :: Env
      environment = { logLevel: Dev, baseUrl: (BaseURL "https://blindtactics.com") }

      rootComponent :: H.Component Router.Query {} Void Aff
      rootComponent = H.hoist (runAppM environment) Router.component

    -- Run the application
    halogenIO <- runUI rootComponent {} body

    -- Listen to the route changes.
    void $ liftEffect
      $ matchesWith (parse routeCodec) \old new ->
          when (old /= Just new) do
            launchAff_ $ halogenIO.query $ H.mkTell $ Router.Navigate new
