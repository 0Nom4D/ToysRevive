module Server (server, API) where

import qualified API.ServerMessage
import Servant

type API = API.ServerMessage.API

-- | The Controllers for the multiple routes
server :: Server API
server = API.ServerMessage.endpoints
