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
import           Domain.Game.Algebra
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
createGameAPI = GameAPI <$> createPool connectToLocalPSQL PSQL.close 1 120 10 <*> pure initialGame

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

mkRoute "GameAPI" [parseRoutes|
  /character/#UUID CharacterR GET
  /user UserCreateR POST
  /user/#UUID UserR GET
  /user/#UUID/characters UserCharactersR GET
  /user/#UUID/events UserEventsR GET
|]

getCharacterR :: UUID -> Handler GameAPI
getCharacterR charid = runHandlerM $ do
  response <- sub >>= liftIO . evalStateT (runAPIRequest (GetCharacter charid))
  json $ fromAPIResponse response

postUserCreateR :: Handler GameAPI
postUserCreateR = runHandlerM $ do
  body <- jsonBody
  handleRequestBody body >>= json . fromAPIResponse
    where
      handleRequestBody (Left _) = return . Left . Err $ "Unable to parse request."
      handleRequestBody (Right (NewUser email username)) = do
        sub >>= liftIO . evalStateT (runAPIRequest (CreateNewUser email username))

getUserR :: UUID -> Handler GameAPI
getUserR uid = runHandlerM $ do
  response <- sub >>= liftIO . evalStateT (runAPIRequest (GetUser uid))
  json $ fromAPIResponse response

getUserCharactersR :: UUID -> Handler GameAPI
getUserCharactersR uid = runHandlerM $ do
  response <- sub >>= liftIO . evalStateT (runAPIRequest (GetUserCharacters uid))
  json $ fromAPIResponse response

getUserEventsR :: UUID -> Handler GameAPI
getUserEventsR uid = runHandlerM $ do
  response <- sub >>= liftIO . evalStateT (runAPIRequest (GetUserEvents uid))
  json $ fromAPIResponse response

main :: IO ()
main = do
  let port = 3000
  putStrLn $ "Listening on port " ++ show port
  croute <- createGameAPI
  run port $ waiApp $ app croute

app :: GameAPI -> RouteM ()
app r = route r
