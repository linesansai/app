{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module API.User.Types where

import API.Response
import Core.Auth
import Core.Types.Auth
import Core.Utils (tshow)
import Data.Aeson
import qualified Data.Text as T
import Servant.API
import Servant.API.Generic
import Database.User (UserId)
import Data.OpenApi (ToSchema)

type UserAPI =
  "user" :> (GetMe :<|> GetUser)

type GetMe = "me" :> ProtectedWithJWT :> Get '[JSON] (WebResponse GetUserError User)

type GetUser =  Capture "userId" UserId :>  ProtectedWithJWT :>Get '[JSON] (WebResponse GetUserError User)

data User = User
  { id:: UserId,
    name :: T.Text,
    login :: Maybe T.Text,
    email :: Maybe T.Text,
    phoneNumber :: T.Text,
    balance :: Maybe Int
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)


type MinLength = Int

data GetUserError
  = UserNotFound
  deriving (Eq, Show)

instance IsAPIError GetUserError where
  toAPIError = \case
    UserNotFound -> ("UserNotFound", "")

