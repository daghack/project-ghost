{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.User.Algebra where

import           Lens.Micro.Platform
import           GHC.Generics
import           Data.Aeson
import           Data.UUID
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.FromRow
import qualified Data.Text as T

type UserId = UUID

data User =
  User { _uid :: UserId
       , _email :: T.Text
       , _username :: T.Text
  } deriving (Show, Generic)

instance ToJSON User
instance FromJSON User
instance ToRow User where
  toRow (User u e n) = [toField u, toField e, toField n]
instance FromRow User where
  fromRow = User <$> field <*> field <*> field

makeLenses ''User
