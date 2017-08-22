{-# LANGUAGE OverloadedStrings #-}

module Domain.Game.Quests ( quests
                          , LiveQuests
) where

import           Domain.Quest.Algebra
import           Domain.Character.Algebra
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

type LiveQuests = HM.HashMap QuestTag Quest

noRequirements :: QuestRequirements
noRequirements = QuestRequirements Nothing Nothing Nothing Nothing Nothing

basicReward :: QuestRewards
basicReward = QuestRewards (Just 100) (Just 100) Nothing

genericSuccessMsgs :: [T.Text]
genericSuccessMsgs = [ "A trivial task, it proved to be no problem."
                     , "Difficult, but not impossible!"
                     , "It was an interesting adventure, but I muddled through well enough."
  ]

genericFailureMsgs :: [T.Text]
genericFailureMsgs = [ "What a complete and utter failure."
                     , "I'm sorry, I just couldn't cut it."
                     , "That was much harder than I was expecting!"
                     , "Almost had it, so close!"
  ]

maybeAdvantage :: (a -> Double -> QuestChance) -> Maybe a -> Double -> QuestChance
maybeAdvantage _ Nothing _ = return ()
maybeAdvantage f (Just a) d = f a d

negotiateWithGroup :: Maybe CharacterRace -> Maybe ClassType -> QuestChance
negotiateWithGroup r c = do
  maybeAdvantage isRace r 0.1
  maybeAdvantage isClassType c 0.1

negotiateWithTribalGoblins :: Quest
negotiateWithTribalGoblins =
  Quest { _requirements = noRequirements
        , _rewards = basicReward
        , _timeToComplete = 2
        , _description = "A band of wild, unruly goblins have invaded a local tavern! Try to convince them to take their goblining elsewhere."
        , _chanceToSucceed = do
          difficulty Medium
          negotiateWithGroup (Just Goblin) (Just Wizard)
        , _failureMessages = [ "I tried to burn the esablishment down, the goblins still inside. The owner did not appreciate that."
                             , "I attempted to clear out the goblins with a nest of venomous vipers. This proved to be a poor decision."
                             , "There were simply too many goblins, and they were far too inebriated. I *definitely* didn't join them."
          ]
        , _successMessages = [ "I successfully lured out the goblins with the promise of a wild whiskey-cow."
                             , "You wouldn't believe what I had to go through, but I finally managed, despite all the goblin feces."
                             , "A rather simple affair, I think. All in all, everything went smoothly."
          ]
  }
clearLocalTavern :: Quest
clearLocalTavern =
  Quest { _requirements = noRequirements
        , _rewards = basicReward
        , _timeToComplete = 1
        , _description = "A band of wild, unruly drunks have invaded a local tavern! Try to convince them to take their nonsense elsewhere."
        , _chanceToSucceed = do
          difficulty Easy
          negotiateWithGroup Nothing Nothing
        , _failureMessages = genericFailureMsgs
        , _successMessages = genericSuccessMsgs
  }

quests :: LiveQuests
quests = HM.fromList [ ("goblinTavern", negotiateWithTribalGoblins)
                     , ("localTavern", clearLocalTavern)
  ]
