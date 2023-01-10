module Server where
import Core.AppHandle
import Servant.Server
import API (API)
import Core.AppMonad (MonadApp, runAppMonad, AppMonad, withAppHandleIO)
import Core.Types.Auth (ProtectedServantJWTCtx)
import qualified API.Auth.Handlers as Auth
import Data.Data (Proxy(..))
import Control.Monad.Except (ExceptT(..))
import UnliftIO (try, liftIO)
import qualified Network.Wai.Handler.Warp as Warp
import Servant.Auth.Server
import qualified API.Register.Handlers as Register
import Servant
import qualified API.User.Handlers as User
import Core.Types.Config (Config(..))
import Data.OpenApi (OpenApi)
import Servant.OpenApi (toOpenApi)
import qualified API.Offer.Handlers as Offer
import qualified Network.Wai.Handler.WarpTLS as Warp

type SwaggerAPI = "swagger.json" :> Get '[JSON] OpenApi

type APIWithSwagger =  API :<|>  SwaggerAPI

swagger :: OpenApi
swagger = toOpenApi (Proxy @API)
  
handlers :: ServerT API AppMonad
handlers = Auth.handlers 
    :<|> Register.handlers
    :<|> User.handlers
    :<|> Offer.handlers

handlersWithSwagger :: ServerT APIWithSwagger AppMonad
handlersWithSwagger = handlers :<|> pure swagger

hoistServerHandler :: AppHandle -> ServerT APIWithSwagger AppMonad -> Server APIWithSwagger
hoistServerHandler ah =
  hoistServerWithContext 
    (Proxy :: Proxy APIWithSwagger)
    (Proxy :: ProtectedServantJWTCtx)
    (Handler . ExceptT . try . runAppMonad ah )

runServer :: IO ()
runServer = withAppHandleIO \ah@AppHandle{config=Config{..}}  -> do 
          authKey <- liftIO $ readKey authKeyPath
          let jwtSettings =  defaultJWTSettings authKey
              cfg = defaultCookieSettings :. jwtSettings :. EmptyContext
          Warp.run 80 
            . serveWithContext (Proxy :: Proxy APIWithSwagger) cfg
            . hoistServerHandler ah
            $ handlersWithSwagger

    where
        warpSettings = Warp.setPort 8080 Warp.defaultSettings
        