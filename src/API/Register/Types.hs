{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module API.Register.Types where

import API.Response
import Core.Auth
import Core.Types.Auth
import Core.Utils (tshow)
import Data.Aeson
import qualified Data.Text as T
import Servant.API
import Servant.API.Generic
import Data.OpenApi (ToSchema)
import Core.Types.UUID (UUID)

type RegisterAPI =
  "register"
    :> ReqBody '[JSON] RegisterRequest
    :> Post '[JSON] (WebResponse RegisterError RegisterResponse)
  :<|> 
   "confirmEmail"
    :> Capture "confirmationId" UUID
    :> Get '[JSON] (WebResponse () ())

data RegisterRequest = RegisterRequest
  { login :: T.Text,
    password :: T.Text,
    name :: T.Text,
    email :: T.Text,
    phoneNumber :: T.Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

newtype RegisterResponse = RegisterResponse
  { token :: AuthToken
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

type MinLength = Int

data RegisterError
  = PasswordIsTooShort MinLength
  | NameIsTooShort MinLength
  | LoginIsTooShort MinLength
  | IncorrectEmail
  | IncorrectPhoneNumber
  | EmailIsBusy
  | LoginIsBusy
  deriving (Eq, Show)

instance IsAPIError RegisterError where
  toAPIError = \case
    PasswordIsTooShort minLength -> ("PasswordIsTooShort", "Minimal length is" <> tshow minLength)
    NameIsTooShort minLength -> ("NameIsTooShort", "Minimal length is" <> tshow minLength)
    LoginIsTooShort minLength -> ("LoginIsTooShort", "Minimal length is" <> tshow minLength)
    IncorrectEmail -> ("IncorrectEmail", "")
    IncorrectPhoneNumber -> ("IncorrectPhoneNumber", "")
    EmailIsBusy -> ("EmailIsBusy", "")
    LoginIsBusy -> ("LoginIsBusy", "")

type ConfirmEmailAPI =
  "confirmEmail"
    :> Capture "confirmationId" UUID
    :> Get '[JSON] (WebResponse () ())
