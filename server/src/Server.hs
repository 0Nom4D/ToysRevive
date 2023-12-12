{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
module Server (app, State(..), AppM) where


import Control.Lens
import Data.Swagger
import Servant(Proxy, HasServer (ServerT), Application, NamedRoutes, (:-), Proxy(Proxy), hoistServer, serve, type (:<|>) ((:<|>)))
import Servant.Swagger
import Servant.Swagger.UI
import App (State(..), AppM)
import qualified API.ServerMessage
import GHC.Generics (Generic)
import Control.Monad.Trans.Reader (runReaderT)

data API mode = API
    { serverMessage :: mode :- NamedRoutes API.ServerMessage.ServerMessageAPI
    }
    deriving stock (Generic)

type NamedAPI = NamedRoutes API

app :: State -> Application
app state = serve proxy $
    hoistServer--WithContext
        proxy
        --(Proxy :: Proxy '[])
        (`runReaderT` state)
        server
    where
        --context = EmptyContext
        proxy = Proxy :: Proxy SwaggerWithAPI

-- | The Controllers for the multiple routes
server :: ServerT SwaggerWithAPI AppM
server = API {
    serverMessage = API.ServerMessage.handler
    } :<|> swaggerSchemaUIServerT appSwagger

-- | Swagger Related
type SwaggerAPI = SwaggerSchemaUI "swagger" "swagger.json"

type SwaggerWithAPI = NamedAPI :<|> SwaggerAPI

-- -- | Swagger spec for our API.
appSwagger :: Swagger
appSwagger =
    toSwagger (Proxy :: Proxy NamedAPI)
        & info . title .~ "ToysRevive API"
        & info . version .~ "1.0"
        & info . license ?~ ("MIT" & url ?~ URL "http://mit.com")
