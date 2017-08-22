module Utility.Postgres where
import           Control.Exception
import           Data.Either
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Data.ByteString.Char8 as BSC

defHandleQuery :: (ToRow q, FromRow r) => PSQL.Connection -> PSQL.Query -> q -> IO (Either String [r])
defHandleQuery conn q v =
  catches (Right <$> PSQL.query conn q v) [ Handler fEHandler
                                , Handler qEHandler
                                , Handler rEHandler
                                , Handler sEHandler ]
    where
      fEHandler = return . Left . PSQL.fmtMessage
      qEHandler = return . Left . PSQL.qeMessage
      rEHandler = return . Left . PSQL.errMessage
      sEHandler = return . Left . BSC.unpack . PSQL.sqlErrorMsg

connectToLocalhost :: IO PSQL.Connection
connectToLocalhost =
  PSQL.connect $
    PSQL.ConnectInfo "localhost" 5432 "postgres" "mysecretpassword" "projectghost"

