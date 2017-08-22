{-# LANGUAGE QuasiQuotes #-}

module Domain.User.Postgres where

import           Control.Exception
import           Data.Either
import           Data.Maybe
import           Data.UUID
import           Data.UUID.V4
import           Domain.User.Algebra
import           Database.PostgreSQL.Simple.SqlQQ
import           Lens.Micro.Platform
import           Utility.Postgres
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BSC

createUserTable :: PSQL.Connection -> IO ()
createUserTable conn =
  PSQL.execute_ conn [sql|
    CREATE TABLE users(uid UUID PRIMARY KEY, email TEXT UNIQUE, username TEXT UNIQUE)
  |] >> return ()

insertNewUser :: PSQL.Connection -> T.Text -> T.Text -> IO (Either String UserId)
insertNewUser conn email username = do
  uid <- nextRandom
  fmap (PSQL.fromOnly . head) <$> defHandleQuery conn [sql|
    INSERT INTO USERS VALUES(?, ?, ?) RETURNING uid
  |] (uid, email, username)

getUserById :: PSQL.Connection -> UUID -> IO (Maybe User)
getUserById conn uid = do
  listToMaybe <$> PSQL.query conn [sql|
    SELECT * FROM users WHERE uid = ?
  |] (PSQL.Only uid)
