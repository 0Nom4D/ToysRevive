module Server (server, API, APIWithSwagger) where

import qualified API.ServerMessage
import Control.Lens
import Data.Swagger
import Servant
import Servant.Swagger

type API = API.ServerMessage.API

type APIWithSwagger = SwaggerAPI :<|> API

-- | The Controllers for the multiple routes
server :: Server APIWithSwagger
server = return appSwagger :<|> API.ServerMessage.endpoints

-- | Swagger Related

-- | API for serving @swagger.json@.
type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

-- | Swagger spec for our API.
appSwagger :: Swagger
appSwagger =
    toSwagger (Proxy :: Proxy API)
        & info . title .~ "ToysRevive API"
        & info . version .~ "1.0"
        & info . license ?~ ("MIT" & url ?~ URL "http://mit.com")
