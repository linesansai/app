module Core.Telegram where

import Control.Exception (SomeException)
import Control.Monad.RWS (liftIO)
import Core.AppHandle (AppHandle (..))
import Core.AppMonad (MonadApp, withAppHandle)
import qualified Core.Types.Config  as C
import qualified Data.Aeson  as J
import Data.Text (Text, unpack)
import qualified Network.HTTP.Client  as HTTP
import UnliftIO (liftIO, tryAny)

sendTelegramMsg ::
  (MonadApp m) =>
  Text ->
  m ()
sendTelegramMsg msg = do
  res <- withAppHandle \AppHandle {..} -> tryAny $ do
    let C.TelegramConfig {..} = C.telegramConfig config
    let url = unpack $ "https://api.telegram.org/bot" <> telegramBotToken <> "/sendMessage"
    initReq <- liftIO $ HTTP.parseRequest url

    let body = J.object ["chat_id" J..= telegramChatId, "text" J..= msg]
        req =
          initReq
            { HTTP.method = "POST",
              HTTP.requestBody = HTTP.RequestBodyLBS $ J.encode body,
              HTTP.requestHeaders = [("Content-Type", "application/json")]
            }
    liftIO $ HTTP.httpLbs req httpManager
    pure ()
  case res of 
    Left e -> liftIO $ print e
    Right () -> pure ()
  pure ()
