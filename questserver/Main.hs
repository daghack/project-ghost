{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Main where
import           Control.Monad.State.Strict
import           Control.Monad.IO.Class
import           Data.UUID
import           Data.Pool
import           Domain.API
import           Domain.Character.Algebra
import           Domain.Character.Postgres (createCharacterTable)
import           Domain.Game.Algebra
import           Domain.Game.Postgres (createEventTable)
import           Domain.Quest.Algebra
import           Domain.Quest.Postgres (createActiveQuestTable)
import           Domain.User.Algebra
import           Domain.User.Postgres (createUserTable)
import           GHC.Generics
import           Network.HTTP.Types (status200, status404)
import           Network.Wai.Handler.Warp (run)
import           Wai.Routes
import           Web.PathPieces
import qualified Data.Aeson as A
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Data.Text as T

localPSQL :: PSQL.ConnectInfo
localPSQL = PSQL.ConnectInfo "localhost" 5432 "postgres" "mysecretpassword" "projectghost"

connectToLocalPSQL :: IO PSQL.Connection
connectToLocalPSQL = PSQL.connect localPSQL

createGameAPI :: IO GameAPI
createGameAPI = do
  pool <- createPool connectToLocalPSQL PSQL.close 1 120 10
  withResource pool $ \conn -> do
    createUserTable conn
    createEventTable conn
    createCharacterTable conn
    createActiveQuestTable conn
  return $ GameAPI pool initialGame

instance PathPiece UUID where
  fromPathPiece = fromText
  toPathPiece = toText

data NewUser =
  NewUser { email :: T.Text, username :: T.Text } deriving (Show, Generic)
instance A.ToJSON NewUser
instance A.FromJSON NewUser

fromAPIResponse :: ResponseResult -> A.Value
fromAPIResponse (Left a) = A.toJSON a
fromAPIResponse (Right a) = A.toJSON a

apiAction :: (MonadIO m) => APIRequest -> GameAPI -> m (Either Err APIResponse)
apiAction req = liftIO . evalStateT (runAPIRequest req)

apiGetActionRoute :: APIRequest -> Handler GameAPI
apiGetActionRoute req = runHandlerM $ sub >>= apiAction req >>= json . fromAPIResponse

mkRoute "GameAPI" [parseRoutes|
  /character/#CharacterId CharacterR GET
  /character/#CharacterId/quests CharacterQuestsR GET
  -- #TODO /character/#CharacterId/classes CharacterClassesR GET
  -- #TODO /character/#CharacterId/dismiss CharacterDismissR GET
  /character/#CharacterId/dispatch/#QuestTag CharacterDispatchR GET

  /user UserCreateR POST
  /user/#UserId UserR GET
  /user/#UserId/characters UserCharactersR GET
  /user/#UserId/events UserEventsR GET

  /delete/#UserId/#EventId DeleteUserEventR GET -- #TODO Change to `/event/#EventId DeleteUserEventR DELETE`
|]

getCharacterR :: CharacterId -> Handler GameAPI
getCharacterR = apiGetActionRoute . GetCharacter

getCharacterQuestsR :: CharacterId -> Handler GameAPI
getCharacterQuestsR = apiGetActionRoute . GetCharacterQuests

getCharacterDispatchR :: CharacterId -> QuestTag -> Handler GameAPI
getCharacterDispatchR = ((.) . (.)) apiGetActionRoute GetCharacterDispatch

postUserCreateR :: Handler GameAPI
postUserCreateR = runHandlerM $ do
  body <- jsonBody
  handleRequestBody body >>= json . fromAPIResponse
    where
      handleRequestBody (Left _) = return . Left . Err $ "Unable to parse request."
      handleRequestBody (Right (NewUser email username)) = do
        sub >>= liftIO . evalStateT (runAPIRequest (CreateNewUser email username))

getUserR :: UserId -> Handler GameAPI
getUserR = apiGetActionRoute . GetUser

getUserCharactersR :: UserId -> Handler GameAPI
getUserCharactersR = apiGetActionRoute . GetUserCharacters

getUserEventsR :: UserId -> Handler GameAPI
getUserEventsR = apiGetActionRoute . GetUserEvents

getDeleteUserEventR :: UserId -> EventId -> Handler GameAPI
getDeleteUserEventR = ((.) . (.)) apiGetActionRoute DeleteUserEvent

main :: IO ()
main = do
  let port = 3000
  putStrLn $ "Listening on port " ++ show port
  croute <- createGameAPI
  run port $ waiApp $ app croute

app :: GameAPI -> RouteM ()
app r = route r
