{-# LANGUAGE DeriveAnyClass #-}
module API.Response where
import qualified Data.Text as T
import Data.Aeson ( ToJSON, FromJSON )
import GHC.Generics (Generic)
import Core.Utils (tshow)
import Data.OpenApi (ToSchema)

type ErrorCode = T.Text
type ErrorDecsription = T.Text

data WebResponse e a = WebResponse
    {   success :: Bool,
        errorCode :: Maybe ErrorCode,
        errorDescription :: Maybe ErrorDecsription,
        body :: Maybe a
    } deriving (Generic, FromJSON, ToJSON, ToSchema)

data ResponseSuccess a = ResponseSuccess 
    {   success :: Bool,
        body :: a
    } deriving (Generic, FromJSON, ToJSON)

class IsAPIError a where 
    toAPIError :: a -> (ErrorCode, ErrorDecsription)

withDefaultCode :: Show a => a -> (a -> ErrorDecsription) -> (ErrorCode, ErrorDecsription)
withDefaultCode a f = (tshow a, f a) 

failWith :: IsAPIError e => e -> WebResponse e a
failWith e = failWith'Plain (toAPIError e)

failWith'Plain :: (ErrorCode, ErrorDecsription) -> WebResponse e a
failWith'Plain (errorCode, errorDescription) = WebResponse {
    success = False,
    errorCode = Just errorCode,
    errorDescription = Just errorDescription,
    body = Nothing
}

successWith :: a -> WebResponse e a
successWith a = WebResponse
    {
        success = True,
        errorCode = Nothing,
        errorDescription = Nothing,
        body = Just a
    }

emptySuccess :: WebResponse e a
emptySuccess = WebResponse
    {
        success = True,
        errorCode = Nothing,
        errorDescription = Nothing,
        body = Nothing
    }