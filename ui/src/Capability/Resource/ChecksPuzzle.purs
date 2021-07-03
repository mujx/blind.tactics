module Blind.Capability.Resource.ChecksPuzzle where

import Prelude
import Blind.Data.Tactics.Puzzle (ChecksPuzzle)
import Data.Either (Either)
import Halogen (HalogenM, lift)

type ChecksPuzzleOptions
  = { nbWhite :: Int
    , nbBlack :: Int
    }

class
  Monad m <= ManageChecksPuzzle m where
  getNextPuzzle :: ChecksPuzzleOptions -> m (Either String ChecksPuzzle)

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance manageTacticsPuzzleHalogenM ::
  ManageChecksPuzzle m =>
  ManageChecksPuzzle (HalogenM st act slots msg m) where
  getNextPuzzle opts = lift $ getNextPuzzle opts
