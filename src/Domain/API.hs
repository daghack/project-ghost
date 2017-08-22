{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.API where

import           Control.Monad.IO.Class
import           Control.Monad.State.Strict
import           Data.Aeson
import           Data.Either
import           Data.Pool
import           Data.UUID
import           Domain.Quest.Algebra
import           Domain.Character.Postgres
import           Domain.User.Algebra
import           Domain.User.Postgres
import           Domain.Game.Algebra
import           Domain.Game.Postgres
import           GHC.Generics
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PSQL

data Err = Err { error :: T.Text } deriving (Show, Generic)

instance ToJSON Err
instance FromJSON Err

data GameAPI = GameAPI (Pool PSQL.Connection) Game
type APIState = StateT GameAPI IO

data APIRequest =
               GetCharacter UUID
             | CreateNewUser T.Text T.Text
             | GetUser UUID
             | GetUserCharacters UUID
             | GetUserEvents UUID

data APIResponse = GetCharacterResp CharRecord
              | CreateNewUserResp NewUserResponse
              | GetUserResp User
              | GetUserCharactersResp [CharRecord]
              | GetUserEventsResp [(EventId, Event)]
  deriving (Show, Generic)

instance ToJSON APIResponse
instance FromJSON APIResponse

type ResponseResult = Either Err APIResponse

data NewUserResponse = NewUserResponse { newuid :: UUID } deriving (Show, Generic)

instance ToJSON NewUserResponse
instance FromJSON NewUserResponse

runAPIRequest :: APIRequest -> APIState ResponseResult
runAPIRequest (GetCharacter charid) = do
  (GameAPI pool _) <- get
  maybechar <- liftIO $ withResource pool $ \conn -> getCharacterByCharacterId conn charid
  genResponse maybechar
    where
      genResponse Nothing = return . Left $ Err "No character matching id."
      genResponse (Just resp) = return . Right . GetCharacterResp $ resp

runAPIRequest (CreateNewUser email username) = do
  (GameAPI pool _) <- get
  maybenewuid <- liftIO $ withResource pool $ \conn -> insertNewUser conn email username
  genResponse maybenewuid
    where
      genResponse (Left _) = return . Left  $ Err "Username or email already registered."
      genResponse (Right r) = return . Right . CreateNewUserResp . NewUserResponse $ r

runAPIRequest (GetUser uid) = do
  (GameAPI pool _) <- get
  maybeuser <- liftIO $ withResource pool $ \conn -> getUserById conn uid
  genResponse maybeuser
    where
      genResponse Nothing = return . Left . Err $ "No user matching that User Id"
      genResponse (Just a) = return . Right . GetUserResp $ a

runAPIRequest (GetUserCharacters uid) = do
  (GameAPI pool _) <- get
  characters <- liftIO $ withResource pool $ \conn -> getCharactersByUserId conn uid
  return . Right . GetUserCharactersResp $ characters

runAPIRequest (GetUserEvents uid) = do
  (GameAPI pool _) <- get
  events <- liftIO $ withResource pool $ \conn -> getEventsByUserId conn uid
  return . Right . GetUserEventsResp $ events
