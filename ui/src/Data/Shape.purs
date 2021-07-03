module Blind.Data.Shape where

import Data.Maybe (Maybe(..))

type Shape
  = { orig :: String
    , dest :: Maybe String
    , brush :: String
    }

mkShape :: String -> String -> Shape
mkShape orig dest =
  { orig: orig
  , dest: Just dest
  , brush: "green"
  }
