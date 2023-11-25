module API.ServerMessage (API, endpoints, ServerMessage(ServerMessage), messageEndpoint) where

import Servant
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)

-- | The Routes of the API
type API = Get '[JSON] ServerMessage
endpoints :: Server API
endpoints = return messageEndpoint

-- | A Controller for a single endpoint
messageEndpoint :: ServerMessage
messageEndpoint = ServerMessage "Hello World"

-- | Data Model
data ServerMessage = ServerMessage {
    message :: String
} deriving (Eq, Show, Generic)

-- | We need to be able to convert the data into JSON
instance ToJSON ServerMessage