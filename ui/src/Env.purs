module Blind.Env where

import Prelude
import Blind.Api.Request (BaseURL)

type Env
  = { logLevel :: LogLevel
    , baseUrl :: BaseURL
    }

data LogLevel
  = Dev
  | Prod

derive instance eqLogLevel :: Eq LogLevel

derive instance ordLogLevel :: Ord LogLevel
