module Gateways.Email where

import Control.Exception
import Core.AppHandle
import Core.AppMonad
import Core.Types.Config
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Database.User (UserId)
import qualified Network.Mail.Mime as Mime
import qualified Network.Mail.SMTP as SMTP
import UnliftIO (liftIO, tryAny)

sendEmail :: MonadApp m => Mime.Mail -> m ()
sendEmail emailMime  = withAppHandle \AppHandle {config = Config {..}, ..} -> do
  let SMTPConfig {..} = smtpConfig
  liftIO $ SMTP.sendMailWithLoginTLS
            (T.unpack smtpHostName)
            (T.unpack smtpUserName)
            (T.unpack smtpPassword)
            emailMime

mkHtmlEmail :: MonadApp m => HtmlEmail -> m Mime.Mail
mkHtmlEmail mail = withAppHandle \AppHandle {config = Config {..}, ..} -> do
  pure $
    Mime.Mail
      { mailFrom = Mime.Address (Just "App") serviceEmail,
        mailTo = [Mime.Address Nothing (mailTo mail)],
        mailCc = [],
        mailBcc = [],
        mailHeaders = [("Subject", subject mail)],
        mailParts = filter (not . null) [[htmlPart]]
      }
  where
    htmlPart =
      Mime.Part
        { partType = "text/html; charset=utf-8",
          partEncoding = Mime.None,
          partDisposition = Mime.DefaultDisposition,
          partContent = Mime.PartContent $ content mail,
          partHeaders = []
        }

data HtmlEmail = HtmlEmail
  { subject :: T.Text,
    content :: LBS.ByteString,
    mailTo :: T.Text
  }