module Core.AppHandle where

import Control.Monad.Reader (MonadReader (ask))
import qualified Data.Map as Map
import GHC.Conc (TVar)
import Core.Types.Auth 
import Database.Persist.Sql (SqlBackend)
import Data.Pool (Pool)
import Core.Types.Config
import Core.Types.UUID (UUID)
import Database.User (UserId)

data AppHandle = AppHandle
    {
        config :: Config,
        authSessions :: TVar (Map.Map AuthSessionId AuthSession),
        emailVerificationSessions :: TVar (Map.Map UUID UserId),
        dbPool :: Pool SqlBackend
    }

