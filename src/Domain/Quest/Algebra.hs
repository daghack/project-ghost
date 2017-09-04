{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Quest.Algebra where

import           Control.Monad.Random
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Trans (lift)
import           Data.Aeson
import           Data.UUID
import           Database.PostgreSQL.Simple.FromRow
import           Domain.Character.Algebra
import           Domain.Character.Attributes
import           GHC.Generics
import           Lens.Micro.Platform
import           Utility.CondList
import qualified Data.Text as T

type QuestTag = T.Text

type TimeRemaining = Int

data QuestRewards =
  QuestRewards { _experience :: Maybe Int
               , _resources :: Maybe Int
               , _charactertraits :: Maybe [T.Text]
  } deriving (Show, Generic)

instance ToJSON QuestRewards
instance FromJSON QuestRewards

makeLenses ''QuestRewards

data QuestDifficulty = Easy | Medium | Hard | Impossible
  deriving (Show, Eq)

data QuestResult =
    QuestFailure { _failuremsg :: T.Text }
  | QuestSuccess { _successmsg :: T.Text, _reward :: QuestRewards }
  deriving (Show, Generic)

instance ToJSON QuestResult
instance FromJSON QuestResult

data QuestInternal =
  QuestInternal { _successprob :: Double
                , _questcharacter :: Character
  } deriving (Show)
defaultQuestInternal = QuestInternal 0.0

makeLenses ''QuestInternal

type QuestChance = State QuestInternal ()

charHasAttr :: CharacterAttr -> Character -> Bool
charHasAttr attr char = view attrset char `hasAttr` attr

addQuestAttr :: CharacterAttr -> QuestChance
addQuestAttr attr = do
  questcharacter . attrset %= flip addAttr attr

removeQuestAttr :: CharacterAttr -> QuestChance
removeQuestAttr attr = do
  questcharacter . attrset %= flip removeAttr attr

difficulty :: QuestDifficulty -> QuestChance
difficulty Easy = successprob += 0.75
difficulty Medium = successprob += 0.50
difficulty Hard = successprob += 0.25
difficulty Impossible = successprob += 0.0

charMeetsCondition :: (Character -> Bool) -> Double -> QuestChance
charMeetsCondition cond prob = do
  meetsCond <- gets (cond . view questcharacter)
  if meetsCond
    then successprob += prob
    else return ()

charEqGetter :: (Eq a) => Getting a Character a -> a -> Double -> QuestChance
charEqGetter l a = charMeetsCondition ((==a) . view l)
charNEqGetter :: (Eq a) => Getting a Character a -> a -> Double -> QuestChance
charNEqGetter l a = charMeetsCondition ((/=a) . view l)

isRace :: CharacterRace -> Double -> QuestChance
isRace = charEqGetter race

isNotRace :: CharacterRace -> Double -> QuestChance
isNotRace = charNEqGetter race

isGender :: Gender -> Double -> QuestChance
isGender = charEqGetter gender

isNotGender :: Gender -> Double -> QuestChance
isNotGender = charNEqGetter gender

isClassType :: ClassType -> Double -> QuestChance
isClassType = charEqGetter (charclass . classtype)

isNotClassType :: ClassType -> Double -> QuestChance
isNotClassType = charNEqGetter (charclass . classtype)

data Quest =
  Quest { _requirements :: ConditionChain Character
        , _rewards :: QuestRewards
        , _timeToComplete :: Int
        , _description :: T.Text
        , _chanceToSucceed :: QuestChance
        , _failureMessages :: [T.Text]
        , _successMessages :: [T.Text]
  }

makeLenses ''Quest

data CharRecord =
  CharRecord { _charid :: UUID
             , _userid :: UUID
             , _character :: Character
             , _currentquest :: Maybe QuestTag
             , _timeremaining :: Maybe TimeRemaining
} deriving (Show, Generic)

makeLenses ''CharRecord

instance ToJSON CharRecord
instance FromJSON CharRecord
instance FromRow CharRecord where
  fromRow = CharRecord <$> field <*> field <*> fromRow <*> field <*> field

attemptQuest :: (MonadRandom m) => Character -> Quest -> m (Character, QuestResult)
attemptQuest char quest
  | not $ char `meetsRequirementsFor` quest = do
    failmsg <- fromList $ zip (quest ^. failureMessages) (repeat 1.0)
    return $ (char, QuestFailure failmsg)
  | otherwise = do
    let qi' = execState (quest ^. chanceToSucceed) (defaultQuestInternal char)
    let char' = qi' ^. questcharacter
    r <- getRandomR (0.0, 1.0)
    failmsg <- fromList $ zip (quest ^. failureMessages) (repeat 1.0)
    succmsg <- fromList $ zip (quest ^. successMessages) (repeat 1.0)
    if qi' ^. successprob < r
      then return $ (char', QuestFailure failmsg)
      else return $ (char', QuestSuccess succmsg (quest ^. rewards))

meetsRequirementsFor :: Character -> Quest -> Bool
meetsRequirementsFor char quest = meetsAll char (quest ^. requirements)
