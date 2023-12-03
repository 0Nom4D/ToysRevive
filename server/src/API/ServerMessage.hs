module API.ServerMessage (API, endpoints, ServerMessage (ServerMessage), messageEndpoint) where

import Control.Lens
import Data.Aeson (ToJSON, toJSON)
import Data.Swagger
import GHC.Generics (Generic)
import Servant

-- | The Routes of the API
type API = Get '[JSON] ServerMessage

endpoints :: Server API
endpoints = return messageEndpoint

-- | A Controller for a single endpoint
messageEndpoint :: ServerMessage
messageEndpoint = ServerMessage "Hello World"

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
