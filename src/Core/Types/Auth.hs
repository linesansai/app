{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Core.Types.Auth where

import Data.Aeson
import qualified Data.Text as T
import Core.Types.UUID (UUID)
import Database.User (UserId)
import Servant.Auth.Server
import GHC.Generics (Generic)
import Data.Data (Proxy (Proxy))
import Data.OpenApi 
import Servant.OpenApi 
import Servant (type (:>))
import Data.Function ((&))
import qualified Data.HashMap.Strict.InsOrd as IHM
import Control.Lens ((<>~))

type ProtectedWithJWT = Auth '[JWT] AuthenticatedUser

type ProtectedServantJWTCtx = Proxy '[CookieSettings, JWTSettings]

instance (HasOpenApi a) => HasOpenApi (ProtectedWithJWT :> a) where
  toOpenApi _ =
    toOpenApi (Proxy :: Proxy a)
      & components . securitySchemes  <>~ secs
      & allOperations . security <>~ secReqs
    where
      secs = SecurityDefinitions $ IHM.fromList [("JWTSecurity", scheme)]
      scheme =
        SecurityScheme ( SecuritySchemeHttp $ HttpSchemeBearer (Just "jwt")) Nothing
      secReqs = [SecurityRequirement $ IHM.fromList [("JWTSecurity", [])]]


newtype AuthSessionId = AuthSessionId UUID
  deriving newtype  (Eq, Ord, FromJSON, ToJSON, ToSchema)

newtype AuthConfirmationCode = AuthConfirmationCode T.Text
  deriving newtype  (Eq, Show, FromJSON, ToJSON, ToSchema)

newtype AuthToken = AuthToken T.Text
  deriving newtype  (FromJSON, ToJSON, ToSchema)

data AuthSession =  AuthSession
  { userId :: UserId,
    code :: AuthConfirmationCode
  } deriving Show

newtype AuthenticatedUser
  = AuthenticatedClient {- ClientID -} Int
  deriving (Show, Generic)

instance ToJWT AuthenticatedUser

instance FromJWT AuthenticatedUser

getUserId :: AuthenticatedUser -> Int
getUserId (AuthenticatedClient userId) = userId

instance ToJSON AuthenticatedUser where
  toJSON (AuthenticatedClient clientId) =
    object ["role" .= ("client" :: String), "clientId" .= clientId]


instance FromJSON AuthenticatedUser where
  parseJSON =
    withObject "AuthenticatedUser" $ \obj -> do
      role <- obj .: "role"
      case (role :: String) of
        "client" -> AuthenticatedClient <$> obj .: "clientId"
        r -> fail $ "Can't parse AuthenticatedUser. Got " <> r

type instance
  BasicAuthCfg =
    BasicAuthData -> IO (AuthResult AuthenticatedUser)

instance FromBasicAuthData AuthenticatedUser where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData