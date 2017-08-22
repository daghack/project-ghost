{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Quest.Algebra where

import           Control.Monad.State.Strict
import           Control.Monad.Trans (lift)
import           Control.Monad.Reader
import           Control.Monad.Random
import           Data.Aeson
import           Data.UUID
import           Domain.Character.Algebra
import           GHC.Generics
import           Lens.Micro.Platform
import           Database.PostgreSQL.Simple.FromRow
import qualified Data.Text as T

type QuestTag = T.Text

type TimeRemaining = Int

data QuestRequirements =
  QuestRequirements { _reqclass  :: Maybe CharacterClass
                    , _reqprof   :: Maybe CharacterProf
                    , _reqrace   :: Maybe CharacterRace
                    , _reqgender :: Maybe Gender
                    , _reqstats  :: Maybe Statblock
  } deriving (Show)

makeLenses ''QuestRequirements

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
  } deriving (Show)
defaultQuestInternal = QuestInternal 0.0

makeLenses ''QuestInternal

type QuestChance = StateT QuestInternal (Reader Character) ()

difficulty :: QuestDifficulty -> QuestChance
difficulty Easy = successprob += 0.75
difficulty Medium = successprob += 0.50
difficulty Hard = successprob += 0.25
difficulty Impossible = successprob += 0.0

charMeetsCondition :: (Character -> Bool) -> Double -> QuestChance
charMeetsCondition cond prob = do
  meetsCond <- lift $ asks cond
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
  Quest { _requirements :: QuestRequirements
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

attemptQuest :: (MonadRandom m) => Character -> Quest -> m QuestResult
attemptQuest char quest
  | not $ char `meetsRequirementFor` quest = do
    failmsg <- fromList $ zip (quest ^. failureMessages) (repeat 1.0)
    return $ QuestFailure failmsg
  | otherwise = do
    let chanceToSucceedReader = execStateT (quest ^. chanceToSucceed) defaultQuestInternal
    let qi' = runReader chanceToSucceedReader char
    r <- getRandomR (0.0, 1.0)
    failmsg <- fromList $ zip (quest ^. failureMessages) (repeat 1.0)
    succmsg <- fromList $ zip (quest ^. successMessages) (repeat 1.0)
    if qi' ^. successprob < r
      then return $ QuestFailure failmsg
      else return $ QuestSuccess succmsg (quest ^. rewards)

meetsRequirementFor :: Character -> Quest -> Bool
meetsRequirementFor char quest =
  and [ meetsClass char $ quest ^. requirements . reqclass
      , meetsProf char $ quest ^. requirements . reqprof
      , meetsRace char $ quest ^. requirements . reqrace
      , meetsGender char $ quest ^. requirements . reqgender
      , meetsStatblock char $ quest ^. requirements . reqstats ]
    where
      meetsClass _ Nothing = True
      meetsClass char (Just c) =
        char ^. charclass . classtype == c ^. classtype &&
        char ^. charclass . classlevel <= c ^. classlevel
      meetsProf _ Nothing = True
      meetsProf char (Just p) =
        char ^. charprof . proftype == p ^. proftype &&
        char ^. charprof . proflevel == p ^. proflevel
      meetsRace _ Nothing = True
      meetsRace char (Just r) = char ^. race == r
      meetsGender _ Nothing = True
      meetsGender char (Just g) =
        char ^. gender == g
      meetsStatblock _ Nothing = True
      meetsStatblock char (Just s) =
        let charStatList = fromStatblock $ char ^. basestats
            reqStatList = fromStatblock s in
          and $ zipWith (>=) charStatList reqStatList
