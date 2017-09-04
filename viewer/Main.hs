{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Maybe
import           Data.Monoid
import           Data.Text.Lazy (toStrict)
import           Data.UUID
import           Domain.API
import           Domain.Character.Algebra
import           Domain.Game.Algebra
import           Domain.Quest.Algebra
import           Domain.User.Algebra
import           Lens.Micro.Platform
import           Network.Wai.Handler.Warp (run)
import           Network.Wreq
import           Text.Blaze.Html.Renderer.Text
import           Viewer.Html
import           Wai.Routes
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as Html

data Viewer = Viewer UserId
createViewer :: String -> IO Viewer
createViewer str = return . Viewer . fromJust . fromString $ str

str_uid :: String
str_uid = "f2793f4e-8a4f-4baf-871e-973484c02599"

mkRoute "Viewer" [parseRoutes|
  /characters CharactersR GET
  /events EventsR GET
|]

getCharactersR :: Handler Viewer
getCharactersR = runHandlerM $ do
  (Viewer uid) <- sub
  response <- liftIO $ get ("http://localhost:3000/user/" <> toString uid <> "/characters")
  let (Just (GetUserCharactersResp chars)) = decode $ response ^. responseBody
  html . toStrict . renderHtml $ charactersPage chars

getEventsR :: Handler Viewer
getEventsR = runHandlerM $ do
  (Viewer uid) <- sub
  response <- liftIO $ get ("http://localhost:3000/user/" <> toString uid <> "/events")
  let (Just (GetUserEventsResp events)) = decode $ response ^. responseBody
  html . toStrict . renderHtml $ eventsPage uid events

main :: IO ()
main = do
  let port = 8080
  viewer <- createViewer str_uid
  run port $ waiApp $ app viewer

app :: Viewer -> RouteM ()
app r = route r
