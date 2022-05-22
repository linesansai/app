{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module API.Auth.Types where

import API.Response
import Data.Aeson
import qualified Data.Text as T
import Servant.API
import Servant.API.Generic
import Core.Auth
import Core.Types.Auth 
import Data.OpenApi (ToSchema)

type AuthAPI = "auth" :> (RequestCodeAPI :<|> ConfirmCodeAPI)  

type RequestCodeAPI = 
    "requestCode"
    :> ReqBody '[JSON] RequestCodeRequest
    :> Post '[JSON] (WebResponse RequestCodeError RequestCodeResponse)

data RequestCodeRequest = RequestCodeRequest
  { login :: T.Text,
    password :: T.Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

newtype RequestCodeResponse = RequestCodeResponse 
    {
        authSessionId :: AuthSessionId
    }deriving  (Generic, FromJSON, ToJSON, ToSchema)

data RequestCodeError = WrongLoginOrPassword
  deriving (Eq, Show)

instance IsAPIError RequestCodeError where 
  toAPIError a = withDefaultCode a \case 
    WrongLoginOrPassword -> ""

type ConfirmCodeAPI = "confirmCode"
    :> ReqBody '[JSON] ConfirmCodeRequest
    :> Post '[JSON] (WebResponse ConfirmCodeError ConfirmCodeResponse)

data ConfirmCodeRequest = 
        ConfirmCodeRequest {
            authSessionId :: AuthSessionId,
            code :: AuthConfirmationCode
        }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

newtype ConfirmCodeResponse = 
    ConfirmCodeResponse
        {
            token :: AuthToken
        }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data ConfirmCodeError = CodeWasNotSent | WrongCode
  deriving (Eq, Show)

instance IsAPIError ConfirmCodeError where 
  toAPIError a = withDefaultCode a \case 
    CodeWasNotSent -> "You need to request code first"
    WrongCode -> ""