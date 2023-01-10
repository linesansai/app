module Core.AppMonad where

import Control.Monad.Logger (MonadLoggerIO, NoLoggingT, runNoLoggingT)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT (runReaderT))
import Core.AppHandle (AppHandle (..))
import Core.Types.Config
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as LBS
import Data.Pool (Pool, destroyAllResources)
import qualified Data.Text.Encoding  as T
import Database.Persist.Postgresql (createPostgresqlPool, createPostgresqlPoolModified)
import Database.Persist.Sql
import GHC.Conc (newTVar, newTVarIO)
import qualified Network.HTTP.Client as HTTP
import UnliftIO (MonadUnliftIO, bracket)

type MonadApp m = (MonadReader AppHandle m, MonadUnliftIO m)

type AppMonad = ReaderT AppHandle IO

withAppHandleIO :: (AppHandle -> IO b) -> IO b
withAppHandleIO action = do
  config <- readConfig
  authSessions <- newTVarIO mempty
  emailVerificationSessions <- newTVarIO mempty
  httpManager <- HTTP.newManager HTTP.defaultManagerSettings
  withLoggedDbPool config \dbPool -> do
    let ah =
          AppHandle
            { config,
              authSessions,
              emailVerificationSessions,
              dbPool,
              httpManager
            }
    action ah

runAppMonadIO :: AppMonad a -> IO a
runAppMonadIO action =
  withAppHandleIO (`runAppMonad` action)

runAppMonad :: AppHandle -> AppMonad a -> IO a
runAppMonad ah action = runReaderT action ah

withLoggedDbPool ::
  Config ->
  (Pool SqlBackend -> IO a) ->
  IO a
withLoggedDbPool config action = do
  bracket
    (liftIO $ runNoLoggingT $ createPostgresqlPool connStr 10)
    releasePool
    action
  where
    connStr = createPgConnString config
    releasePool = liftIO . destroyAllResources

-- createPgConnString :: Config -> IO ConnectionString
createPgConnString Config {dbConfig = DatabaseConfig {..}} = do
  let proto = "postgresql://"
   in T.encodeUtf8 $
        proto <> user <> ":" <> pass <> "@" <> host <> ":" <> port <> "/" <> name

withAppHandle :: MonadApp m => (AppHandle -> m b) -> m b
withAppHandle action = ask >>= action

runDB :: MonadApp m => SqlPersistM a -> m a
runDB action = withAppHandle \AppHandle {..} -> liftIO $ runSqlPersistMPool action dbPool