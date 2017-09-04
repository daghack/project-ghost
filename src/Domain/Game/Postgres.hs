{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Game.Postgres where

import           Control.Monad
import           Control.Monad.State.Strict
import           Data.Aeson
import           Data.Maybe
import           Data.UUID
import           Data.UUID.V4
import           Domain.Character.Postgres
import           Domain.Character.Algebra
import           Domain.Game.Algebra
import           Domain.Quest.Algebra
import           Domain.Quest.Postgres
import           Domain.User.Algebra
import           Domain.User.Postgres
import           Database.PostgreSQL.Simple.SqlQQ
import           Lens.Micro.Platform
import           Utility.Postgres
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Data.ByteString.Lazy as BS

createEventTable :: PSQL.Connection -> IO ()
createEventTable conn =
  PSQL.execute_ conn [sql| CREATE TABLE IF NOT EXISTS events( id UUID PRIMARY KEY DEFAULT gen_random_uuid(), uid UUID, event TEXT ) |] >> return ()

createNewEvent :: PSQL.Connection -> UserId -> Event -> IO EventId
createNewEvent conn uid event = do
  head . map PSQL.fromOnly <$> PSQL.query conn [sql|
    INSERT INTO events VALUES (DEFAULT, ?, ?) RETURNING id
  |] (uid, event)

getEventsByUserId :: PSQL.Connection -> UserId -> IO [(EventId, Event)]
getEventsByUserId conn uid =
  map handleResponse <$> PSQL.query conn [sql|
    SELECT * FROM events WHERE uid = ?
  |] (PSQL.Only uid)
    where
      handleResponse :: (EventId, UserId, BS.ByteString) -> (EventId, Event)
      handleResponse (eid, uid, bs) = (eid, fromJust $ decode bs)

deleteUserEvent :: PSQL.Connection -> UserId -> EventId -> IO ()
deleteUserEvent conn uid eid = PSQL.execute conn [sql| DELETE FROM events WHERE id = ? AND uid = ? |] (eid, uid) >> return ()

runQuests :: Game -> PSQL.Connection -> IO [EventId]
runQuests game conn = do
  completedQuests <- tickActiveQuests conn
  catMaybes <$> forM completedQuests (handleEventTrigger game)
    where
      handleEventTrigger :: Game -> (CharacterId, QuestTag) -> IO (Maybe EventId)
      handleEventTrigger game (cid, qt) = do
        charrecord <- getCharacterByCharacterId conn cid
        case charrecord of
          Nothing -> return Nothing
          Just cr -> do
            let questResultM = (cr ^. character) `attemptQuestTag` qt
            questResult <- evalStateT questResultM game
            updateCharacter conn cid (fst questResult)
            Just <$> createNewEvent conn (cr ^. userid) (CharacterReturnFromQuest cid $ snd questResult)

runTest :: IO ()
runTest = do
  conn <- connectToLocalhost
  let thisuid = fromJust $ fromString "3295112f-6db3-4741-9fdb-42034c67992c"
  (Right cid) <- insertRandomCharacter conn thisuid "luatest.lua" ""
  (Right cid2) <- insertRandomCharacter conn thisuid "luatest.lua" ""
  insertActiveQuest conn cid "localTavern" 1
  insertActiveQuest conn cid2 "localTavern" 2
  runQuests initialGame conn
  return ()
