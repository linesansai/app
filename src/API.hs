module API where
import qualified API.Auth.Types as Auth
import qualified API.Register.Types as Register
import Servant (type (:<|>))
import qualified API.User.Types as User
import qualified API.Offer.Types as Offer

type API = Auth.AuthAPI 
    :<|> Register.RegisterAPI
    :<|> User.UserAPI
    :<|> Offer.OfferAPI

