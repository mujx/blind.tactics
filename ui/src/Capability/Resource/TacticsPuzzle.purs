module Blind.Capability.Resource.TacticsPuzzle where

import Prelude
import Blind.Data.Tactics.Puzzle (Puzzle)
import Data.Either (Either)
import Halogen (HalogenM, lift)

class
  Monad m <= ManageTacticsPuzzle m where
  getNextPuzzle :: Int -> Int -> m (Either String Puzzle)

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance manageTacticsPuzzleHalogenM ::
  ManageTacticsPuzzle m =>
  ManageTacticsPuzzle (HalogenM st act slots msg m) where
  getNextPuzzle rating movesToFollow = lift $ getNextPuzzle rating movesToFollow
