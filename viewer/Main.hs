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
import           Wai.Routes
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as Html

charactersPage :: [CharRecord] -> Html.Html
charactersPage crs = Html.docTypeHtml $ do
  Html.body $ forM_ crs renderCharRecord
    where
      renderCharacter :: Character -> Html.Html
      renderCharacter c = do
          let line1 = T.unwords [c ^. name . title, c ^. name . lastname <> ",", c ^. name . firstname]
          let line2 = unwords [show $ c ^. gender, show $ c ^. race, show $ c ^. charclass . classtype]
          Html.div $ Html.toHtml (T.unpack line1)
          Html.div $ Html.toHtml line2
      renderCharRecord :: CharRecord -> Html.Html
      renderCharRecord cr = Html.div Html.! setAvailableClass cr $ renderCharacter (cr ^. character)
      setAvailableClass cr =
        case cr ^. currentquest of
          Nothing -> Html.class_ "available"
          Just _ -> Html.class_ "unavailable"

data Viewer = Viewer UserId
createViewer :: String -> IO Viewer
createViewer str = return . Viewer . fromJust . fromString $ str

str_uid :: String
str_uid = "f2793f4e-8a4f-4baf-871e-973484c02599"

mkRoute "Viewer" [parseRoutes|
  /characters CharactersR GET
|]

getCharactersR :: Handler Viewer
getCharactersR = runHandlerM $ do
  (Viewer uid) <- sub
  response <- liftIO $ get ("http://localhost:3000/user/" <> toString uid <> "/characters")
  let (Just (GetUserCharactersResp chars)) = decode $ response ^. responseBody
  html . toStrict . renderHtml $ charactersPage chars

main :: IO ()
main = do
  let port = 8080
  viewer <- createViewer str_uid
  run port $ waiApp $ app viewer

app :: Viewer -> RouteM ()
app r = route r
