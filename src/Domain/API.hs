{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.API where

import           Control.Monad.IO.Class
import           Control.Monad.State.Strict
import           Data.Aeson
import           Data.Either
import           Data.Maybe
import           Data.Pool
import           Data.UUID
import           Domain.Character.Algebra
import           Domain.Character.Postgres
import           Domain.Game.Algebra
import           Domain.Game.Postgres
import           Domain.Quest.Algebra
import           Domain.Quest.Postgres
import           Domain.User.Algebra
import           Domain.User.Postgres
import           GHC.Generics
import           Lens.Micro.Platform
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Data.HashMap.Strict as HM

data Err = Err { error :: T.Text } deriving (Show, Generic)

instance ToJSON Err
instance FromJSON Err

data GameAPI = GameAPI (Pool PSQL.Connection) Game
type APIState = StateT GameAPI IO

data APIRequest =
               GetCharacter CharacterId
             | GetCharacterQuests CharacterId
             | GetCharacterDispatch CharacterId QuestTag
             | CreateNewUser T.Text T.Text
             | GetUser UserId
             | GetUserCharacters UserId
             | GetUserEvents UserId
             | DeleteUserEvent UserId EventId

data APIResponse =
                GetCharacterResp CharRecord
              | GetCharacterQuestsResp [QuestTag]
              | GetCharacterDispatchResp
              | CreateNewUserResp NewUserResponse
              | GetUserResp User
              | GetUserCharactersResp [CharRecord]
              | GetUserEventsResp [(EventId, Event)]
              | DeleteUserEventResp
  deriving (Show, Generic)

instance ToJSON APIResponse
instance FromJSON APIResponse

type ResponseResult = Either Err APIResponse

data NewUserResponse = NewUserResponse { newuid :: UUID } deriving (Show, Generic)

instance ToJSON NewUserResponse
instance FromJSON NewUserResponse

runAPIRequest :: APIRequest -> APIState ResponseResult
runAPIRequest (GetCharacter cid) = do
  (GameAPI pool _) <- get
  maybechar <- liftIO $ withResource pool $ \conn -> getCharacterByCharacterId conn cid
  genResponse maybechar
    where
      genResponse Nothing = return . Left $ Err "No character matching id."
      genResponse (Just resp) = return . Right . GetCharacterResp $ resp

runAPIRequest (GetCharacterQuests cid) = do
  (GameAPI pool game) <- get
  maybechar <- liftIO $ withResource pool $ \conn -> getCharacterByCharacterId conn cid
  genResponse game maybechar
    where
      genResponse game Nothing = return . Left $ Err "No character matching id."
      genResponse game (Just crec) =
        return . Right . GetCharacterQuestsResp . HM.keys . HM.filter (meetsRequirementsFor $ crec ^. character) $ game ^. livequests

runAPIRequest (GetCharacterDispatch cid qt) = do
  (GameAPI pool game) <- get
  maybecharrec <- liftIO $ withResource pool $ flip getCharacterByCharacterId cid
  let maybechar = view character <$> maybecharrec
  let maybequest = game ^? livequests . ix qt
  if
    | isNothing $ game ^? livequests . ix qt ->
      return . Left . Err $ "No such quest available."
    | isNothing maybecharrec ->
      return . Left . Err $ "No such character to send."
    | isJust (maybecharrec >>= view currentquest) ->
      return . Left . Err $ "Character is already on a quest."
    | fromJust $ not <$> (meetsRequirementsFor <$> maybechar <*> maybequest) ->
      return . Left . Err $ "Character does not meet requirements for this quest."
    | otherwise -> do
      liftIO $ withResource pool $ \conn ->
        insertActiveQuest conn cid qt (fromJust $ game ^? livequests . at qt . _Just . timeToComplete)
      return . Right $ GetCharacterDispatchResp

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

runAPIRequest (DeleteUserEvent uid eid) = do
  (GameAPI pool _) <- get
  liftIO $ withResource pool $ \conn -> deleteUserEvent conn uid eid
  return . Right $ DeleteUserEventResp
