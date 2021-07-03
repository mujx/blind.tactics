module Blind.Data.FFI.Chartist where

import Effect (Effect)
import Data.Maybe (Maybe(..))
import Prelude

data Chart

updateChart :: Maybe Chart -> Array Int -> Effect Unit
updateChart (Just chart) series = update chart series
updateChart Nothing      _      = pure unit

destroyChart :: Maybe Chart -> Effect Unit
destroyChart (Just chart) = detach chart
destroyChart Nothing      = pure unit

foreign import mkLineChart :: String -> Array Int -> Effect Chart

foreign import update :: Chart -> Array Int -> Effect Unit

foreign import detach :: Chart -> Effect Unit
