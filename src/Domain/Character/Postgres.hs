{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Character.Postgres where

import           Control.Exception
import           Control.Monad.Random
import           Data.Aeson
import           Data.Either
import           Data.Maybe
import           Data.UUID
import           Data.UUID.V4
import           Database.PostgreSQL.Simple ((:.)(..))
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.SqlQQ
import           Domain.Character.Algebra
import           Domain.Character.Attributes
import           Domain.Character.Random
import           Domain.Quest.Algebra
import           Domain.User.Algebra
import           GHC.Generics
import           Lens.Micro.Platform
import           Utility.Postgres
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PSQL

createCharacterTable :: PSQL.Connection -> IO ()
createCharacterTable conn =
  PSQL.execute_ conn [sql|
    CREATE TABLE IF NOT EXISTS
      characters( id UUID PRIMARY KEY DEFAULT gen_random_uuid()
                , uid UUID
                , firstname TEXT
                , lastname TEXT
                , title TEXT
                , gender INT
                , classtype INT
                , classlevel INT
                , proftype INT
                , proflevel INT
                , race INT
                , stat1 INT
                , stat2 INT
                , stat3 INT
                , stat4 INT
                , stat5 INT
                , stat6 INT
                , attrset TEXT[]
                )
  |] >> return ()

mkTestUser :: IO User
mkTestUser = do
  uuid <- nextRandom
  return $ User uuid "nolat301@gmail.com" "daghack"

testchar =
  Character { _name = CharacterName "Talon" "Bowler" "Squire"
            , _gender = Male
            , _charclass = CharacterClass Wizard 100
            , _charprof = CharacterProf Soldier 5
            , _race = Human
            , _basestats = Statblock 10 10 10 10 10 10
            , _attrset = fromAttrList ["charming", "goblinfriend"]
  }

insertCharacter :: PSQL.Connection -> UserId -> Character -> IO (Either String CharacterId)
insertCharacter conn userid char = do
  uuid <- nextRandom
  fmap (PSQL.fromOnly . head) <$> defHandleQuery conn [sql|
    INSERT INTO characters VALUES(DEFAULT, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    RETURNING id
  |] ([userid] :. char)

updateCharacter :: PSQL.Connection -> CharacterId -> Character -> IO ()
updateCharacter conn cid char =
  PSQL.execute conn [sql|
    UPDATE characters
      SET firstname = ?
        , lastname = ?
        , title = ?
        , gender = ?
        , classtype = ?
        , classlevel = ?
        , proftype = ?
        , proflevel = ?
        , race = ?
        , stat1 = ?
        , stat2 = ?
        , stat3 = ?
        , stat4 = ?
        , stat5 = ?
        , stat6 = ?
        , attrset = ?
      WHERE id = ?
  |] (char :. [toString cid]) >> return ()

insertRandomCharacter :: PSQL.Connection -> UserId -> FilePath -> String -> IO (Either String CharacterId)
insertRandomCharacter conn user luafile varprefix = do
  char <- genRandomCharacter luafile varprefix
  insertCharacter conn user char

getCharactersByUserId :: PSQL.Connection -> UserId -> IO [CharRecord]
getCharactersByUserId conn uid =
  PSQL.query conn [sql|
    SELECT characters.*, activequests.questtag, activequests.timeremaining FROM characters
      LEFT OUTER JOIN activequests ON (characters.id = activequests.charid)
      WHERE uid = ?
  |] (PSQL.Only uid)

getCharacterByCharacterId :: PSQL.Connection -> CharacterId -> IO (Maybe CharRecord)
getCharacterByCharacterId conn charid =
  listToMaybe <$> PSQL.query conn [sql|
    SELECT characters.*, activequests.questtag, activequests.timeremaining FROM characters
      LEFT OUTER JOIN activequests ON (characters.id = activequests.charid)
      WHERE id = ?
  |] (PSQL.Only charid)

getUserByCharacterId :: PSQL.Connection -> CharacterId -> IO (Maybe UserId)
getUserByCharacterId conn cid =
  fmap PSQL.fromOnly . listToMaybe <$> PSQL.query conn [sql|
    SELECT uid FROM characters WHERE id = ?
  |] (PSQL.Only cid)

getAllCharacters :: PSQL.Connection -> IO [CharRecord]
getAllCharacters conn =
  PSQL.query_ conn [sql|
    SELECT characters.*, activequests.questtag, activequests.timeremaining FROM characters
      LEFT OUTER JOIN activequests ON (characters.id = activequests.charid)
  |]
