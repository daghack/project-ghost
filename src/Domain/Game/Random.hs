module Domain.Game.Random where

import           Control.Monad.Trans (lift)
import           Control.Monad.Random
import           Domain.Character.Algebra
import           Domain.Character.Random
import           Domain.Game.Algebra

randomGameCharacter :: FilePath -> String -> GameStateM Character
randomGameCharacter filepath varname = lift $ genRandomCharacter filepath varname
