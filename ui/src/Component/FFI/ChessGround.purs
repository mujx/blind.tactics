module Blind.Component.FFI.ChessGround where

import Prelude
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Data.Foldable (sequence_)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Undefinable (Undefinable, toUndefinable, toMaybe)
import Blind.Data.Shape (Shape)
import Blind.Data.ProblemInput (Orientation(..))
import Effect (Effect)

type ShapeUndefinable
  = { orig :: String
    , dest :: Undefinable String
    , brush :: String
    }

type ShapeConfig
  = { shapes :: Array Shape
    , enabled :: Boolean
    }

type BoardConfig
  = { fen :: String
    , animation :: { duration :: Int, enabled :: Boolean }
    , highlight :: { lastMove :: Boolean }
    , orientation :: Orientation
    , coordinates :: Boolean
    , viewOnly :: Boolean
    , resisable :: Boolean
    , drawable :: ShapeConfig
    , disableContextMenu :: Boolean
    , draggable :: { enabled :: Boolean }
    , movable :: { free :: Boolean }
    }

initConfig :: BoardConfig
initConfig =
  { fen: ""
  , orientation: White
  , highlight:
      { lastMove: false
      }
  , animation:
      { duration: 700
      , enabled: true
      }
  , viewOnly: false
  , resisable: true
  , disableContextMenu: true
  , coordinates: false
  , movable:
      { free: false
      }
  , draggable:
      { enabled: false
      }
  , drawable:
      { enabled: true
      , shapes: []
      }
  }

moveDelay :: Milliseconds
moveDelay = Milliseconds 700.0

data ChessBoard

derive instance genericChessBoard :: Generic ChessBoard _

instance showChessBoard :: Show ChessBoard where
  show = genericShow

replayMoves :: Maybe ChessBoard -> Array Shape -> Effect Unit
replayMoves Nothing _ = pure unit

replayMoves (Just b) shapes = launchAff_ $ sequence_ $ map (playMove b) shapes
  where
  playMove :: ChessBoard -> Shape -> Aff Unit
  playMove board shape = do
    liftEffect $ move board shape
    delay moveDelay

destroy :: Maybe ChessBoard -> Effect Unit
destroy board = case board of
  Just b -> destroyBoard b
  Nothing -> pure unit

setShapes :: Maybe ChessBoard -> Array Shape -> Effect Unit
setShapes board shapes = case board of
  Nothing -> pure unit
  Just b -> setShapes_ b (map (\x -> x { dest = toUndefinable x.dest }) shapes)

getShapes :: ChessBoard -> Array Shape
getShapes b = map (\x -> x { dest = toMaybe x.dest }) $ getShapes_ b

move :: ChessBoard -> Shape -> Effect Unit
move board shape = case shape.dest of
  Just d -> move_ board shape.orig d
  Nothing -> pure unit

removeShapes :: Maybe ChessBoard -> Effect Unit
removeShapes board = case board of
  Just b -> setShapes_ b []
  Nothing -> pure unit

setConfig :: Maybe ChessBoard -> BoardConfig -> Effect Unit
setConfig board cfg = case board of
  Just b -> setConfig_ b (cfg { orientation = show cfg.orientation })
  Nothing -> pure unit

mkChessGround :: String -> BoardConfig -> Effect ChessBoard
mkChessGround elem cfg = mkChessGround_ elem (cfg { orientation = show cfg.orientation })

foreign import mkChessGround_ :: forall e. String -> { orientation :: String | e } -> Effect ChessBoard

foreign import setConfig_ :: forall e. ChessBoard -> { orientation :: String | e } -> Effect Unit

foreign import setShapes_ :: ChessBoard -> Array ShapeUndefinable -> Effect Unit

foreign import getShapes_ :: ChessBoard -> Array ShapeUndefinable

foreign import destroyBoard :: ChessBoard -> Effect Unit

foreign import addResizeListener :: Effect Unit

foreign import removeResizeListener :: Effect Unit

foreign import move_ :: ChessBoard -> String -> String -> Effect Unit
