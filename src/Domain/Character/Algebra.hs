{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Character.Algebra where

import           Control.Monad (replicateM)
import           Control.Monad.Random
import           Data.Aeson
import           Data.UUID
import           Data.Maybe
import           Data.Monoid
import           GHC.Generics
import           Lens.Micro.Platform
import qualified Data.Text as T
import qualified Data.Map.Lazy as M
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow

type CharacterId = UUID

data QuestStatus = Available | Questing
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON QuestStatus
instance FromJSON QuestStatus
instance FromField QuestStatus where
  fromField a b = toEnum <$> fromField a b
instance ToField QuestStatus where
  toField = toField . fromEnum

data Gender = Male | Female
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON Gender
instance FromJSON Gender
instance FromField Gender where
  fromField a b = toEnum <$> fromField a b
instance ToField Gender where
  toField = toField . fromEnum

data CharacterRace = Human | Elf | Dwarf | Goblin
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON CharacterRace
instance FromJSON CharacterRace
instance FromField CharacterRace where
  fromField a b = toEnum <$> fromField a b
instance ToField CharacterRace where
  toField = toField . fromEnum

data CharacterName =
  CharacterName { _firstname :: T.Text
                , _lastname :: T.Text
                , _title :: T.Text
  } deriving (Show, Generic)

instance ToJSON CharacterName
instance FromJSON CharacterName
instance FromRow CharacterName where
  fromRow = CharacterName <$> field <*> field <*> field
instance ToRow CharacterName where
  toRow (CharacterName a b c) = toField <$> [a, b, c]

type NameList = M.Map CharacterRace (M.Map Gender [T.Text])

getNameFromNameList :: [T.Text] -> NameList -> CharacterRace -> Gender -> [T.Text]
getNameFromNameList def nl r g =
  let dl = M.lookup r nl >>= M.lookup g in
  fromMaybe def dl


data ClassType = Fighter | Ranger | Wizard
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON ClassType
instance FromJSON ClassType
instance FromField ClassType where
  fromField a b = toEnum <$> fromField a b
instance ToField ClassType where
  toField = toField . fromEnum

data CharacterClass =
  CharacterClass { _classtype :: ClassType
                 , _classlevel :: Int
  } deriving (Show, Generic)

instance ToJSON CharacterClass
instance FromJSON CharacterClass
instance FromRow CharacterClass where
  fromRow = CharacterClass <$> field <*> field
instance ToRow CharacterClass where
  toRow (CharacterClass a b) = [toField a, toField b]

data ProfType = Miner | Logger | Soldier
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON ProfType
instance FromJSON ProfType
instance FromField ProfType where
  fromField a b = toEnum <$> fromField a b
instance ToField ProfType where
  toField = toField . fromEnum

data CharacterProf =
  CharacterProf { _proftype :: ProfType
                      , _proflevel :: Int
  } deriving (Show, Generic)

instance ToJSON CharacterProf
instance FromJSON CharacterProf
instance FromRow CharacterProf where
  fromRow = CharacterProf <$> field <*> field
instance ToRow CharacterProf where
  toRow (CharacterProf a b) = [toField a, toField b]

data Statblock =
  Statblock { _wis :: Int
            , _int :: Int
            , _con :: Int
            , _dex :: Int
            , _cha :: Int
            , _str :: Int
  } deriving (Show, Generic)

fromStatblock :: Statblock -> [Int]
fromStatblock s = [ _wis
                  , _int
                  , _con
                  , _dex
                  , _cha
                  , _str ] <*> pure s

toStatblock :: [Int] -> Statblock
toStatblock l =
  Statblock { _wis = l !! 0
            , _int = l !! 1
            , _con = l !! 2
            , _dex = l !! 3
            , _cha = l !! 4
            , _str = l !! 5 }

instance ToJSON Statblock
instance FromJSON Statblock
instance FromRow Statblock where
  fromRow = Statblock <$> field <*> field <*> field <*> field <*> field <*> field
instance ToRow Statblock where
  toRow = map toField . fromStatblock

data Character =
  Character { _name        :: CharacterName
            , _gender      :: Gender
            , _charclass   :: CharacterClass
            , _charprof    :: CharacterProf
            , _race        :: CharacterRace
            , _basestats   :: Statblock
  } deriving (Show, Generic)

instance ToJSON Character
instance FromJSON Character
instance FromRow Character where
  fromRow = Character <$>
              fromRow <*>
              field   <*>
              fromRow <*>
              fromRow <*>
              field   <*>
              fromRow
instance ToRow Character where
  toRow (Character n g c p r s) =
    toRow n <> [toField g] <> toRow c <> toRow p <> [toField r] <> toRow s

concat <$> mapM makeLenses [ ''CharacterName
                           , ''CharacterClass
                           , ''CharacterProf
                           , ''Statblock
                           , ''Character
                           ]
