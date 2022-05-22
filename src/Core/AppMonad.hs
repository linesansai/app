module Core.AppMonad where

import Control.Monad.Reader (MonadReader (ask),ReaderT (runReaderT), MonadIO (liftIO))
import Core.AppHandle (AppHandle (..))
import Database.Persist.Sql
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson (decode)
import GHC.Conc (newTVar, newTVarIO)
import Core.Types.Config
import Data.Pool (Pool, destroyAllResources)
import Database.Persist.Postgresql (createPostgresqlPoolModified, createPostgresqlPool)
import qualified Data.Text.Encoding as T
import Control.Monad.Logger (runNoLoggingT, MonadLoggerIO, NoLoggingT)
import UnliftIO (MonadUnliftIO, bracket)

type MonadApp m = (MonadReader AppHandle m, MonadUnliftIO m)

type AppMonad = ReaderT AppHandle IO



withAppHandleIO :: (AppHandle -> IO b) -> IO b
withAppHandleIO action = do
    config <- readConfig
    authSessions <- newTVarIO mempty
    emailVerificationSessions <- newTVarIO mempty
    withLoggedDbPool config \dbPool -> do
        let ah = AppHandle {
            config,
            authSessions,
            emailVerificationSessions,
            dbPool
        }
        action ah

runAppMonadIO ::  AppMonad a -> IO a
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


--createPgConnString :: Config -> IO ConnectionString
createPgConnString Config {dbConfig = DatabaseConfig {..}} = do
  let proto = "postgresql://"
   in T.encodeUtf8 $
    proto <> user <> ":" <> pass <> "@" <> host <> ":" <> port <> "/" <> name

withAppHandle :: MonadApp m => (AppHandle -> m b) -> m b
withAppHandle action = ask >>= action

runDB :: MonadApp m => SqlPersistM a -> m a
runDB action = withAppHandle \AppHandle {..} -> liftIO $ runSqlPersistMPool action dbPool