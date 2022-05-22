module API.Register.Handlers where

import API.Register.Types
import API.Response (WebResponse, failWith, successWith, emptySuccess)
import Control.Monad (when)
import Control.Monad.Except
import Core.AppMonad (MonadApp, runDB, withAppHandle)
import Core.Auth (genJWTToken, getEmailVerificationSession, createEmailVerificationSession)
import qualified Data.Text as T
import Database.Persist ( Entity(Entity) )
import Database.User (CreateUser (..), createUser, loadUserByEmail, loadUserByLogin, userLogin)
import Servant (ServerT, err400,type  (:<|>) (..))
import Data.Char (isDigit)
import Control.Exception (throw)
import qualified Database.User as DB
import Database.Esqueleto.Legacy ( (=.), val )
import Core.Types.UUID (UUID (UUID))
import Gateways.Email (HtmlEmail(..), mkHtmlEmail, sendEmail)
import Data.Aeson (encode)
import Core.AppHandle (AppHandle(..))
import Core.Types.Config (Config(..))
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy as TL

handlers :: MonadApp m => ServerT RegisterAPI m
handlers = register :<|> confirmEmail

register :: MonadApp m => RegisterRequest -> m (WebResponse RegisterError RegisterResponse)
register RegisterRequest {..} =
  case runChecks of
    Left err -> pure $ failWith err
    _ ->
      runDB createUser' >>= \case
        Left (Entity _ existedUser) ->
          pure $
            failWith $
              if userLogin existedUser == login'
                then LoginIsBusy
                else EmailIsBusy
        Right (Entity userId user) -> do
          sendEmailVerification userId user
          token <- genJWTToken userId >>= maybe (error "Unable to create JWT token") pure
          pure $ successWith $ RegisterResponse token
  where
    password' = T.strip password
    email' = T.strip email
    login' = T.strip login
    name' = T.strip name
    phoneNumber' = T.strip phoneNumber
    createUser' =
      createUser
        CreateUser
          { password = password',
            email = email',
            login = login',
            name = name',
            phoneNumber  = phoneNumber'
          }
    runChecks = runExcept do
      checkLength 8 password PasswordIsTooShort
      checkLength 5 login LoginIsTooShort
      checkLength 5 name NameIsTooShort
      checkEmailFormat
      checkPhoneNumberFormat
    checkEmailFormat = case T.splitOn "@" email of
      [l, r] | T.length l > 1 && T.length r > 1 -> pure ()
      _ -> throwError IncorrectEmail
    checkPhoneNumberFormat = case T.unpack phoneNumber' of
      '7' : tail | length tail == 10 && all isDigit tail -> pure ()
      _ -> throwError IncorrectPhoneNumber
    checkLength n field er =
      when (T.length field < n) $
        throwError (er n)
    sendEmailVerification userId DB.User {..}  = withAppHandle \AppHandle {config = Config {..}} ->do 
            UUID sessionId <- createEmailVerificationSession userId
            let link = TL.encodeUtf8 $ TL.fromStrict $ appUrl <> "/confirmEmail/" <> sessionId
            mail <- mkHtmlEmail HtmlEmail {
                subject = "Confirm your emal",
                content = "Follow the link to confirm your email: " <> link,
                mailTo = userEmail
            }
            sendEmail mail

confirmEmail :: MonadApp m => UUID -> m (WebResponse () ())
confirmEmail uuid = do 
  getEmailVerificationSession uuid >>= \case
     Nothing -> throw err400
     Just userId -> do 
       runDB $ DB.updateUser userId [#isEmailConfirmed =. val True]
       pure emptySuccess
