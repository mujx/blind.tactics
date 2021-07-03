-- | We can represent valid locations in our application with a simple sum type. This will cause
-- | any invalid routes to fail at compile-time.
-- |
-- | But since the browser represents locations with strings, we'll also need a way to write our
-- | `Route` type to a string and parse strings into valid `Route` values. It's tedious and error-
-- | prone to maintain separate printing and parsing functions which can fall out of sync with
-- | another, and even worse to write them manually. Fortunately, the `routing-duplex` library will
-- | help us write a bi-directional codec which solves both problems.
-- |
-- | For more information about the library and to read the tutorial, see:
-- | https://github.com/natefaubion/purescript-routing-duplex/tree/v0.2.0
module Blind.Data.Route where

import Prelude hiding ((/))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Routing.Duplex (RouteDuplex', root, segment, string, path)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route
  = Home
  | Visualize
  | Problem String
  | FindChecks
  | KnightPath
  | CommonSquares

derive instance genericRoute :: Generic Route _

derive instance eqRoute :: Eq Route

derive instance ordRoute :: Ord Route

instance shoRoute :: Show Route where
  show = genericShow

routeCodec :: RouteDuplex' Route
routeCodec =
  root
    $ sum
        { "Home": noArgs
        , "Visualize": "visualize" / noArgs
        , "Problem": path "problem" (string segment)
        , "FindChecks": "checks" / noArgs
        , "KnightPath": "knight-path" / noArgs
        , "CommonSquares": "common-squares" / noArgs
        }
