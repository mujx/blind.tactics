module Blind.Api.Request
  ( Token -- constructor and decoders not exported
  , BaseURL(..)
  , Endpoint(..)
  , RequestMethod(..)
  , RequestOptions(..)
  , getNextTacticsPuzzle
  , getNextChecksPuzzle
  , getNextKnightPathPuzzle
  , getNextCommonSquaresPuzzle
  ) where

import Affjax (printError, request, defaultRequest)
import Affjax.ResponseFormat as RF
import Blind.Data.Tactics.Puzzle
  ( Puzzle
  , puzzleCodec
  , ChecksPuzzle
  , checksPuzzleCodec
  , KnightPathPuzzle
  , knightPathPuzzleCodec
  , CommonSquaresPuzzle
  , commonSquaresPuzzleCodec
  )
import Blind.Capability.Resource.ChecksPuzzle (ChecksPuzzleOptions)
import Data.Argonaut.Core (Json)
import Data.Bifunctor (lmap)
import Data.Codec as Codec
import Data.Codec.Argonaut (printJsonDecodeError)
import Data.Either (Either(..))
import Data.FormURLEncoded (fromArray, encode)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Prelude hiding ((/))
import Routing.Duplex (RouteDuplex', prefix, root, print)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

newtype Token
  = Token String

derive instance eqToken :: Eq Token

derive instance ordToken :: Ord Token

-- | We won't derive a `Show` instance, because we don't ever want to reveal the token.
-- | Instead, we'll provide a manual instance.
instance showToken :: Show Token where
  show (Token _) = "Token {- token -}"

newtype BaseURL
  = BaseURL String

data RequestMethod
  = Get
  | Post (Maybe Json)
  | Put (Maybe Json)
  | Delete

-- | This data type captures each endpoint our API supports. In a larger application this would be
-- | tedious to maintain, and it's more common to generate endpoints from a Swagger or Open API
-- | spec. For the time being, though, we'll take the same approach as we did for our routes and
-- | create an encompassing sum type to represent all endpoints. With this type, requests to
-- | invalid endpoints (endpoints not captured in this type) will fail to compile.
data Endpoint
  = Login
  | NextTacticsPuzzle
  | NextChecksPuzzle
  | NextKnightPathPuzzle
  | NextCommonSquaresPuzzle

derive instance genericEndpoint :: Generic Endpoint _

endpointCodec :: RouteDuplex' Endpoint
endpointCodec =
  root $ prefix "api"
    $ sum
        { "Login": "auth" / "login" / noArgs
        , "NextTacticsPuzzle": "games" / "tactics" / "next" / noArgs
        , "NextChecksPuzzle": "games" / "checks" / "next" / noArgs
        , "NextKnightPathPuzzle": "games" / "knight_path" / "next" / noArgs
        , "NextCommonSquaresPuzzle": "games" / "common_squares" / "next" / noArgs
        }

type RequestOptions
  = { endpoint :: Endpoint
    , method :: RequestMethod
    }

getNextTacticsPuzzle :: forall m. MonadAff m => BaseURL -> Int -> Int -> m (Either String Puzzle)
getNextTacticsPuzzle (BaseURL baseUrl) rating movesToFollow = do
  let
    urlParams =
      fromMaybe ""
        ( encode
            $ fromArray
                [ Tuple "rating" (Just $ show rating)
                , Tuple "moves_to_follow" (Just $ show movesToFollow)
                ]
        )
  res <-
    liftAff $ request
      $ defaultRequest
          { url = baseUrl <> print endpointCodec NextTacticsPuzzle <> "?" <> urlParams
          , method = Left GET
          , responseFormat = RF.json
          }
  case res of
    Left e -> pure $ Left $ printError e
    Right v -> pure $ lmap printJsonDecodeError $ Codec.decode puzzleCodec v.body

getNextChecksPuzzle :: forall m. MonadAff m => BaseURL -> ChecksPuzzleOptions -> m (Either String ChecksPuzzle)
getNextChecksPuzzle (BaseURL baseUrl) opts = do
  let
    urlParams =
      fromMaybe ""
        ( encode
            $ fromArray
                [ Tuple "nb_white" (Just $ show opts.nbWhite)
                , Tuple "nb_black" (Just $ show opts.nbBlack)
                ]
        )
  res <-
    liftAff $ request
      $ defaultRequest
          { url = baseUrl <> print endpointCodec NextChecksPuzzle <> "?" <> urlParams
          , method = Left GET
          , responseFormat = RF.json
          }
  case res of
    Left e -> pure $ Left $ printError e
    Right v -> pure $ lmap printJsonDecodeError $ Codec.decode checksPuzzleCodec v.body

getNextKnightPathPuzzle :: forall m r. MonadAff m => BaseURL -> { maxSteps :: Int | r } -> m (Either String KnightPathPuzzle)
getNextKnightPathPuzzle (BaseURL baseUrl) opts = do
  let
    urlParams = fromMaybe "" (encode $ fromArray [ Tuple "max_steps" (Just $ show opts.maxSteps) ])
  res <-
    liftAff $ request
      $ defaultRequest
          { url = baseUrl <> print endpointCodec NextKnightPathPuzzle <> "?" <> urlParams
          , method = Left GET
          , responseFormat = RF.json
          }
  case res of
    Left e -> pure $ Left $ printError e
    Right v -> pure $ lmap printJsonDecodeError $ Codec.decode knightPathPuzzleCodec v.body

getNextCommonSquaresPuzzle :: forall m r. MonadAff m => BaseURL -> { maxPieces :: Int | r } -> m (Either String CommonSquaresPuzzle)
getNextCommonSquaresPuzzle (BaseURL baseUrl) opts = do
  let
    urlParams = fromMaybe "" (encode $ fromArray [ Tuple "max_pieces" (Just $ show opts.maxPieces) ])
  res <-
    liftAff $ request
      $ defaultRequest
          { url = baseUrl <> print endpointCodec NextCommonSquaresPuzzle <> "?" <> urlParams
          , method = Left GET
          , responseFormat = RF.json
          }
  case res of
    Left e -> pure $ Left $ printError e
    Right v -> pure $ lmap printJsonDecodeError $ Codec.decode commonSquaresPuzzleCodec v.body
