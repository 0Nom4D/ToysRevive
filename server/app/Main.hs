module Main (main) where

import Server
import Servant
import Network.Wai.Handler.Warp (run)

-- | Entrypoint of the binary
main :: IO ()
main = run 8080 application

-- | The Server Application
application :: Application
application = serve (Proxy :: Proxy API) server