--
-- Raw puzzle structure as received through the API.
--
module Blind.Data.Tactics.Puzzle where

import Prelude
import Data.Codec.Argonaut (JsonCodec)
import Data.Maybe (Maybe(..))
import Data.String (take, drop)
import Blind.Data.Shape (Shape)
import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut as CA
import Blind.Data.ProblemInput (ProblemInput, Orientation(..))

type Puzzle
  = { id :: String
    , fen :: String
    , moves :: Array String
    , orientation :: String
    , solution :: String
    , themes :: Array String
    , popularity :: Int
    , nb_plays :: Int
    , rating :: Int
    , rating_deviation :: Int
    , game_url :: String
    }

puzzleCodec :: JsonCodec Puzzle
puzzleCodec =
  CAR.object "Puzzle"
    { id: CA.string
    , fen: CA.string
    , rating: CA.int
    , rating_deviation: CA.int
    , popularity: CA.int
    , nb_plays: CA.int
    , moves: CA.array CA.string
    , themes: CA.array CA.string
    , orientation: CA.string
    , solution: CA.string
    , game_url: CA.string
    }

type CheckMove
  = { to :: String
    , from :: String
    , role :: String
    , move_display :: String
    }

checkMoveCodec :: JsonCodec CheckMove
checkMoveCodec =
  CAR.object "CheckMove"
    { to: CA.string
    , from: CA.string
    , role: CA.string
    , move_display: CA.string
    }

type ChecksPuzzle
  = { fen :: String
    , checks :: Array CheckMove
    , black :: Array String
    , white :: Array String
    }

checksPuzzleCodec :: JsonCodec ChecksPuzzle
checksPuzzleCodec =
  CAR.object "ChecksPuzzle"
    { fen: CA.string
    , checks: CA.array checkMoveCodec
    , black: CA.array CA.string
    , white: CA.array CA.string
    }

type KnightPathPuzzle
  = { from :: String
    , to :: String
    , squares :: Array (Array String)
    , fen :: String
    }

knightPathPuzzleCodec :: JsonCodec KnightPathPuzzle
knightPathPuzzleCodec =
  CAR.object "KnightPathPuzzle"
    { fen: CA.string
    , squares: CA.array (CA.array CA.string)
    , from: CA.string
    , to: CA.string
    }

type CommonSquaresPuzzle
  = { fen :: String
    , pieces :: Array String
    , squares :: Array String
    }

commonSquaresPuzzleCodec :: JsonCodec CommonSquaresPuzzle
commonSquaresPuzzleCodec =
  CAR.object "CommonSquaresPuzzle"
    { fen: CA.string
    , squares: CA.array CA.string
    , pieces: CA.array CA.string
    }

mkMove :: String -> Shape
mkMove s =
  { orig: take 2 s
  , dest: Just $ take 2 $ drop 2 s
  , brush: "green"
  }

mkProblemInput :: Puzzle -> ProblemInput
mkProblemInput p =
  { fen: p.fen
  , moves: map mkMove p.moves
  , orientation: if p.orientation == "White" then White else Black
  , solution: mkMove p.solution
  }
