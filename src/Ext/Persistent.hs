module Ext.Persistent where
import qualified Database.Persist as P
import qualified Database.Persist.Sql as P


keyToInt :: P.ToBackendKey P.SqlBackend record => P.Key record -> Int
keyToInt = fromIntegral . P.fromSqlKey

intToKey :: P.ToBackendKey P.SqlBackend record => Int -> P.Key record
intToKey = P.toSqlKey . fromIntegral