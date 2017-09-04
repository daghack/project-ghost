{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Domain.Character.Attributes where

import           Data.Aeson
import           Data.String (IsString(..))
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.FromField
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Database.PostgreSQL.Simple.Types as PSQLT

data CharacterAttr =
  CharacterAttr T.Text
  deriving (Show, Ord, Eq)

instance IsString CharacterAttr where
  fromString = CharacterAttr . T.toCaseFold . T.pack

instance ToJSON CharacterAttr where
  toJSON (CharacterAttr a) = toJSON a

instance FromJSON CharacterAttr where
  parseJSON bs = CharacterAttr . T.toCaseFold <$> parseJSON bs

instance ToField CharacterAttr where
  toField (CharacterAttr a) = toField a

instance FromField CharacterAttr where
  fromField a b = CharacterAttr <$> fromField a b

type CharacterAttrSet = S.Set CharacterAttr

instance ToField CharacterAttrSet where
  toField = toField . PSQLT.PGArray . S.toList

instance FromField CharacterAttrSet where
  fromField a b = S.fromList . PSQLT.fromPGArray <$> pgArrayFieldParser (fromField) a b

fromAttrList :: [CharacterAttr] -> CharacterAttrSet
fromAttrList = S.fromList

emptyAttrSet :: CharacterAttrSet
emptyAttrSet = S.empty

addAttr :: CharacterAttrSet -> CharacterAttr -> CharacterAttrSet
addAttr = flip S.insert

removeAttr :: CharacterAttrSet -> CharacterAttr -> CharacterAttrSet
removeAttr = flip S.delete

hasAttr :: CharacterAttrSet -> CharacterAttr -> Bool
hasAttr = flip S.member
