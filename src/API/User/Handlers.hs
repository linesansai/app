module API.User.Handlers where

import API.Response (WebResponse, failWith, successWith)
import API.User.Types
import Control.Monad (when)
import Control.Monad.Except
import Core.AppMonad (MonadApp, runDB)
import Core.Auth (genJWTToken, withUserAuth)
import Core.Types.Auth
import qualified Data.Text as T
import Database.Persist
import Database.User (CreateUser (..), createUser, loadUserById, loadUserByEmail, loadUserByLogin, userLogin, UserId)
import qualified Database.User as DB
import Ext.Persistent (intToKey)
import Servant (ServerT,type  (:<|>) (..))
import Servant.Auth.Server (AuthResult)

handlers :: MonadApp m => ServerT UserAPI m
handlers = getMe :<|> getUser

getMe :: MonadApp m => AuthResult AuthenticatedUser -> m (WebResponse GetUserError User)
getMe auth = withUserAuth auth \userId -> do
  runDB (DB.loadUserById userId) >>= \case
    Nothing -> pure $ failWith UserNotFound
    Just (Entity userId DB.User {..}) ->
      pure $
        successWith
          User
            { id = userId,
              name = userName,
              login = Just userLogin,
              email = Just userEmail,
              phoneNumber = userPhoneNumber,
              balance = Just userBalance
            }

getUser :: MonadApp m => UserId -> AuthResult AuthenticatedUser -> m (WebResponse GetUserError User)
getUser userId auth  = withUserAuth auth \_ -> do
  runDB (DB.loadUserById userId) >>= \case
    Nothing -> pure $ failWith UserNotFound
    Just (Entity _ DB.User {..}) ->
      pure $
        successWith
          User
            { id = userId,
              name = userName,
              login = Nothing,
              email = Nothing,
              phoneNumber = userPhoneNumber,
              balance = Nothing
            }