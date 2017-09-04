{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
module Domain.Quest.Postgres where

import           Domain.Quest.Algebra
import           Domain.Character.Algebra
import           Utility.Postgres
import           Database.PostgreSQL.Simple.SqlQQ
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Data.Text as T

createActiveQuestTable :: PSQL.Connection -> IO ()
createActiveQuestTable conn =
  PSQL.execute_ conn [sql|
    CREATE TABLE IF NOT EXISTS activequests( charid UUID PRIMARY KEY, questtag TEXT, timeremaining INT )
  |] >> return ()

insertActiveQuest :: PSQL.Connection -> CharacterId -> QuestTag -> TimeRemaining -> IO ()
insertActiveQuest conn charid qtag time = do
  -- #TODO Ensure Character is not already in the table
  PSQL.execute conn [sql|
    INSERT INTO activequests VALUES (?, ?, ?)
  |] (charid, qtag, time)
  return ()

tickActiveQuests :: PSQL.Connection -> IO [(CharacterId, QuestTag)]
tickActiveQuests conn = do
  PSQL.execute_ conn [sql|
    UPDATE activequests SET timeremaining = timeremaining - 1
  |]
  PSQL.query_ conn [sql|
    DELETE FROM activequests WHERE timeremaining <= 0 RETURNING charid, questtag
  |]
