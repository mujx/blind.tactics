--
-- Small module to read puzzles foom a json file.
--
module Blind.Data.ExternalPuzzles where

import Prelude
import Data.Traversable (traverse)
import Foreign.Index ((!))
import Data.List.Types (NonEmptyList)
import Control.Monad.Except (runExcept)
import Foreign (F, Foreign, readString, readInt, readArray, ForeignError)
import Data.Either (Either(..))
import Blind.Data.ProblemInput (ProblemInput)
import Blind.Data.Tactics.Puzzle (Puzzle, mkProblemInput)

readPuzzle :: Foreign -> F Puzzle
readPuzzle value = do
  id <- value ! "id" >>= readString
  fen <- value ! "fen" >>= readString
  moves <- value ! "moves" >>= readArray >>= traverse readString
  themes <- value ! "themes" >>= readArray >>= traverse readString
  solution <- value ! "solution" >>= readString
  orientation <- value ! "orientation" >>= readString
  game_url <- value ! "game_url" >>= readString
  nb_plays <- value ! "nb_plays" >>= readInt
  rating <- value ! "rating" >>= readInt
  rating_deviation <- value ! "rating_deviation" >>= readInt
  popularity <- value ! "popularity" >>= readInt
  pure
    { id
    , nb_plays
    , themes
    , game_url
    , rating
    , popularity
    , rating_deviation
    , fen
    , solution
    , moves
    , orientation
    }

parsePuzzles :: Either (NonEmptyList ForeignError) (Array Puzzle)
parsePuzzles = runExcept $ traverse readPuzzle =<< readArray (puzzles_ 0)

allPuzzles :: Array ProblemInput
allPuzzles = case parsePuzzles of
  Left _ -> []
  Right v -> map mkProblemInput v

foreign import puzzles_ :: Int -> Foreign
