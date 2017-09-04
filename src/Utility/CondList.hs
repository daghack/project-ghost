module Utility.CondList where

import           Data.Monoid
import qualified Control.Monad.Writer.Strict as W

data Cond a = Cond (a -> Bool) | CondList [Cond a]

instance Monoid (Cond a) where
  mempty = CondList []
  mappend (CondList a) (CondList b) = CondList (a <> b)
  mappend b@(CondList _) a = mappend a b
  mappend a (CondList b) = CondList $ a : b
  mappend a b = CondList [a, b]

checkConditions :: a -> Cond a -> [Bool]
checkConditions a (Cond con) = [con a]
checkConditions a (CondList b) = concatMap (checkConditions a) b

type ConditionChain a = W.Writer (Cond a) ()

norequirement :: ConditionChain a
norequirement = return ()

requirement :: (a -> Bool) -> ConditionChain a
requirement = W.tell . Cond

runConditions :: a -> ConditionChain a -> [Bool]
runConditions a b = checkConditions a $ W.execWriter b

meetsSome :: a -> ConditionChain a -> Bool
meetsSome = ((.) . (.)) or runConditions

meetsAll :: a -> ConditionChain a -> Bool
meetsAll = ((.) . (.)) and runConditions
