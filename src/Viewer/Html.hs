{-# LANGUAGE OverloadedStrings #-}

module Viewer.Html where

import           Control.Monad
import           Data.Monoid
import           Data.UUID
import           Domain.Character.Algebra
import           Domain.Game.Algebra
import           Domain.Quest.Algebra
import           Domain.User.Algebra
import           Lens.Micro.Platform
import           Network.Wreq
import           Text.Blaze.Html5 ((!))
import qualified Data.Text as T
import qualified Data.String as Str
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as HtmlA

borderedDiv = Html.div ! HtmlA.style "border:2px solid black;border-radius:5px;"

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
      renderCharRecord cr = borderedDiv Html.! setAvailableClass cr $ renderCharacter (cr ^. character)
      setAvailableClass cr =
        case cr ^. currentquest of
          Nothing -> HtmlA.class_ "available"
          Just _ -> HtmlA.class_ "unavailable"

eventsPage :: UserId -> [(EventId, Event)] -> Html.Html
eventsPage uid events = Html.docTypeHtml $ do
  Html.body $ forM_ events renderEvent
    where
      renderEvent :: (EventId, Event) -> Html.Html
      renderEvent (eid, CharacterReturnFromQuest cid result) =
        borderedDiv $ do
          Html.div . Html.toHtml . toString $ eid
          Html.div . Html.toHtml . toString $ cid
          Html.div . Html.toHtml $ case result of
            QuestFailure msg -> msg
            QuestSuccess msg _ -> msg
          Html.div $
            Html.a ! HtmlA.href (Str.fromString $ "http://localhost:3000/delete/" <> toString uid <> "/" <> toString eid) $ "Delete"
