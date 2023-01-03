module API.Auth.Handlers where 
import API.Auth.Types
import Database.User (loadUserByLoginAndPassword, User (..))
import Core.AppMonad (runDB, MonadApp)
import API.Response (failWith, successWith, WebResponse)
import Core.Auth (createAuthSession, getAuthSession, genJWTToken, deleteAuthSession)
import Database.Persist (Entity(..))
import Core.Types.Auth (AuthSession(..))
import Data.Maybe (fromMaybe)
import Servant
import UnliftIO (liftIO)
import Gateways.Email
import Data.Aeson (encode)
import qualified Database.User as DB
import Database.Esqueleto.Legacy (val, (=.))

handlers :: MonadApp m => ServerT AuthAPI m
handlers = requestCode :<|> confirmCode

requestCode :: MonadApp m => RequestCodeRequest -> m (WebResponse RequestCodeError RequestCodeResponse)
requestCode RequestCodeRequest {..} = do 
    runDB (loadUserByLoginAndPassword login password) >>= \case
       Nothing -> pure $ failWith WrongLoginOrPassword
       Just (Entity userId user) -> do 
           (sessionId, session) <- createAuthSession userId
           sendEmailWithCode user session
           liftIO $ print session
           pure $ successWith $ RequestCodeResponse sessionId
    where 
        sendEmailWithCode User {..} AuthSession {..}  = do 
            mail <- mkHtmlEmail HtmlEmail {
                subject = "Your confirmation code",
                content = "Your confirmation code is " <> encode code,
                mailTo = userEmail
            }
            sendEmail mail
            pure ()

confirmCode :: MonadApp m => ConfirmCodeRequest -> m (WebResponse ConfirmCodeError ConfirmCodeResponse)
confirmCode ConfirmCodeRequest {..} = do 
    getAuthSession authSessionId >>= \case
       Nothing -> pure $ failWith CodeWasNotSent
       Just AuthSession {code = actualCode, ..} 
        | code /= actualCode -> pure $ failWith WrongCode
        | otherwise -> do
            token <- genJWTToken userId >>= maybe (error "Unable to create JWT token") pure
            deleteAuthSession authSessionId
            runDB $ DB.updateUser userId [#isEmailConfirmed =. val True]
            pure $ successWith (ConfirmCodeResponse token)
