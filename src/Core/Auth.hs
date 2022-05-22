module Core.Auth where
import Data.Aeson
import qualified Data.Text as T
import Database.User (UserId)
import System.Random (randomRIO)
import Core.AppMonad (MonadApp, withAppHandle)
import Control.Monad (replicateM)
import Core.AppHandle
import GHC.Conc (atomically, readTVar, writeTVar, TVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import Core.Types.Auth
import Core.Utils (genUUID)
import Servant.Auth.Server (readKey, makeJWT, defaultJWTSettings, AuthResult (Authenticated), generateKey, writeKey)
import Core.Types.Config
import qualified Database.Persist as P
import qualified Database.Persist.Sql as P
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BL
import Data.Either.Combinators (rightToMaybe)
import Ext.Persistent (intToKey)
import Control.Exception (throw)
import Servant (err401)
import Core.Types.UUID (UUID)

genConfirmationCode :: MonadApp m => m AuthConfirmationCode
genConfirmationCode = AuthConfirmationCode . T.pack <$> replicateM 6 (randomRIO ('0', '9'))

genAuthSessionId :: MonadApp m  => m AuthSessionId
genAuthSessionId = AuthSessionId <$> genUUID

writeSession :: (MonadApp m, Ord k) => (AppHandle -> TVar (Map.Map k a)) -> (k, a) -> m ()
writeSession f (k, v)=
  withAppHandle \ah -> do
    liftIO $ atomically do
      sessions <- readTVar (f ah)
      let updatedMap = Map.insert k v sessions
      writeTVar (f ah) updatedMap

getSession :: (MonadApp m, Ord k) => (AppHandle -> TVar (Map.Map k a)) -> k -> m (Maybe a)
getSession f k = withAppHandle \ah -> do
    liftIO $ atomically do
      sessions <- readTVar (f ah)
      let mbSession =  Map.lookup k sessions
      pure mbSession

deleteSession :: (MonadApp m, Ord k) => (AppHandle -> TVar (Map.Map k a)) -> k -> m ()
deleteSession f k = withAppHandle \ah -> do
    liftIO $ atomically do
      sessions <- readTVar (f ah)
      let updatedMap = Map.delete k sessions
      writeTVar (f ah) updatedMap

createEmailVerificationSession ::  MonadApp m => UserId -> m UUID
createEmailVerificationSession userId = do 
  uuid <- genUUID
  writeSession emailVerificationSessions (uuid, userId)
  pure uuid

getEmailVerificationSession :: MonadApp m => UUID -> m (Maybe UserId)
getEmailVerificationSession = getSession emailVerificationSessions

deleteEmailVerificationSession :: MonadApp m => UUID -> m ()
deleteEmailVerificationSession = deleteSession emailVerificationSessions

createAuthSession ::  MonadApp m => UserId -> m (AuthSessionId, AuthSession)
createAuthSession userId = do
  sessionId <- genAuthSessionId
  code <- genConfirmationCode
  let session = AuthSession
        { userId,
          code
        }
  writeSession authSessions (sessionId, session)
  pure (sessionId, session)

getAuthSession :: MonadApp m => AuthSessionId -> m (Maybe AuthSession)
getAuthSession = getSession authSessions

deleteAuthSession :: MonadApp m => AuthSessionId -> m ()
deleteAuthSession = deleteSession authSessions

genJWTToken :: MonadApp m => UserId -> m (Maybe AuthToken)
genJWTToken userId= withAppHandle \AppHandle {config = Config {..}, ..} -> do
  key <- liftIO $ readKey authKeyPath
  token <- rightToMaybe <$> liftIO (makeJWT user (defaultJWTSettings key) Nothing)
  pure $ mkAuthToken <$> token
  where
    mkAuthToken = AuthToken . T.decodeUtf8 . BL.toStrict
    user = AuthenticatedClient $ fromIntegral $ P.fromSqlKey userId

withUserAuth :: (MonadApp m) => AuthResult AuthenticatedUser -> (UserId -> m a) -> m a
withUserAuth auth action = case auth of
  Authenticated (AuthenticatedClient userId) -> action (intToKey userId)
  _ -> liftIO $ throw err401

generateJWK :: FilePath -> IO ()
generateJWK = writeKey 