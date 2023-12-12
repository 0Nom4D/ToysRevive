module Main (main) where

import Network.Wai.Handler.Warp (run)
import Server (app, State(State))
import Database.Connection (buildConnectionURI)
import System.Environment.MrEnv (envAsInt)

-- | Entrypoint of the binary
main :: IO ()
main = do
    connectionURI <- buildConnectionURI
    appPort <- envAsInt "TR_SERVER_PORT" 8080
    run appPort $ app (State undefined)

