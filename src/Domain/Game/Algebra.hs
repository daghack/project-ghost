{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Game.Algebra where

import           Control.Monad.Random
import           Control.Monad.State.Strict
import           Data.Aeson
import           Data.UUID
import           Domain.Character.Algebra
import           Domain.Game.Quests
import           Domain.Quest.Algebra
import           GHC.Generics
import           Lens.Micro.Platform
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
import qualified Data.Text as T
import qualified Foreign.Lua as Lua
import qualified Data.HashMap.Strict as HM

type GameStateM = StateT Game IO

type EventId = UUID

data Event =
  CharacterReturnFromQuest { _cid :: CharacterId
                           , _questresult :: QuestResult
  } deriving (Show, Generic)

instance ToJSON Event
instance FromJSON Event
instance ToField Event where
  toField = toField . encode
instance FromField Event where
  fromField = fromJSONField

data Game =
  Game { _livequests :: LiveQuests
  }

makeLenses ''Game

initialGame :: Game
initialGame = Game quests

gameTick :: Int -> GameStateM ()
gameTick ticks = return ()

attemptQuestTag :: Character -> QuestTag -> GameStateM QuestResult
attemptQuestTag char qt = do
  q <- preuse (livequests . ix qt)
  case q of
    Nothing -> return $ QuestFailure "No such quest available to me."
    Just quest -> char `attemptQuest` quest
