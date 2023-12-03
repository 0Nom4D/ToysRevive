module Main (main) where

import Network.Wai.Handler.Warp (run)
import Servant
import Server

-- | Entrypoint of the binary
main :: IO ()
main = run 8080 application

-- | The Server Application
application :: Application
application = serve (Proxy :: Proxy APIWithSwagger) server
