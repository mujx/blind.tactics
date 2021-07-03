-- | This module exports various utilities for working with a REST API and Json. It also provides
-- | a few helpers shared among requests which I found useful when implementing the production
-- | monad, `Blind.AppM`.
module Blind.Api.Utils where

import Prelude
import Blind.Capability.Log (class Log, logError)
import Blind.Capability.Now (class Now)
import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut (JsonCodec, printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

-- | This small utility decodes JSON and logs any failures that occurred, returning the parsed
-- | value only if decoding succeeded. This utility makes it easy to abstract the mechanices of
-- | dealing with malformed responses. See `Conduit.AppM` for examples of this in practice.
decode :: forall m a. Log m => Now m => JsonCodec a -> Maybe Json -> m (Maybe a)
decode _ Nothing = logError "Response malformed" *> pure Nothing

decode codec (Just json) = case CA.decode codec json of
  Left err -> logError (printJsonDecodeError err) *> pure Nothing
  Right response -> pure (Just response)
