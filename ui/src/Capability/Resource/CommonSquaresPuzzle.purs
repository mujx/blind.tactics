module Blind.Capability.Resource.CommonSquaresPuzzle where

import Prelude
import Blind.Data.Tactics.Puzzle (CommonSquaresPuzzle)
import Data.Either (Either)
import Halogen (HalogenM, lift)

class Monad m <= ManageCommonSquaresPuzzle m where
  getNextPuzzle :: forall r. { maxPieces :: Int | r } -> m (Either String CommonSquaresPuzzle)

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance manageCommonSquarePuzzleHalogenM :: ManageCommonSquaresPuzzle m => ManageCommonSquaresPuzzle (HalogenM st act slots msg m) where
  getNextPuzzle opts = lift $ getNextPuzzle opts
