module Blind.Data.ProblemInput where

import Prelude
import Blind.Data.Shape (Shape)

data BrushColor
  = Green
  | Blue

instance showBrushColor :: Show BrushColor where
  show Green = "green"
  show Blue = "blue"

data Orientation
  = White
  | Black

instance showOrientation :: Show Orientation where
  show White = "white"
  show Black = "black"

type ProblemInput
  = { fen :: String
    , moves :: Array Shape
    , solution :: Shape
    , orientation :: Orientation
    }
