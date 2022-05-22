{-# LANGUAGE DeriveAnyClass #-}
module Core.Types.Config where
import Data.Aeson (FromJSON, decode)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

data Config = Config 
    {
        authKeyPath :: FilePath,
        appUrl :: T.Text,
        dbConfig :: DatabaseConfig,
        smtpConfig :: SMTPConfig,
        serviceEmail :: T.Text
    } deriving (Generic, FromJSON)

data DatabaseConfig = DatabaseConfig
    {
        host :: T.Text,
        name :: T.Text,
        user :: T.Text,
        pass :: T.Text,
        port :: T.Text
    } deriving (Generic, FromJSON)

data SMTPConfig = SMTPConfig 
    {   
        smtpHostName :: T.Text,
        smtpUserName :: T.Text,
        smtpPassword :: T.Text
    } deriving (Generic, FromJSON)

readConfig :: IO Config
readConfig = do 
    configData <- LBS.readFile "./config.local"
    maybe (error "unable to decode config file") pure $ decode configData 