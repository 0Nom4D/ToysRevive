module API.ServerMessage (ServerMessageAPI, handler, ServerMessage (ServerMessage), messageEndpoint) where

import Control.Lens
import Data.Aeson (ToJSON, toJSON)
import Data.Swagger
import GHC.Generics (Generic)
import Servant
import Servant.Server.Generic (AsServerT)
import App (AppM)

-- | The Routes of the API
data ServerMessageAPI mode = ServerMessageAPI {
    endpoint :: mode :- Get '[JSON] ServerMessage
} deriving stock (Generic)

handler :: ServerMessageAPI (AsServerT AppM)
handler = ServerMessageAPI {
    endpoint = messageEndpoint
}

-- | A Controller for a single endpoint
messageEndpoint :: AppM ServerMessage
messageEndpoint = return $ ServerMessage "Hello World"

-- | Data Model
data ServerMessage = ServerMessage
    { message :: String
    }
    deriving (Eq, Show, Generic)

-- | We need to be able to convert the data into JSON
instance ToJSON ServerMessage

-- | Everything to make our type compatible with Swagger
instance ToSchema ServerMessage where
    declareNamedSchema proxy =
        genericDeclareNamedSchema defaultSchemaOptions proxy
            & mapped . schema . description ?~ "A Simple Message from the server"
            & mapped . schema . example ?~ toJSON (ServerMessage "Hello World")
