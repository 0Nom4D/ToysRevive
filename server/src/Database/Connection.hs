module Database.Connection (buildConnectionURI) where
import System.Environment (getEnvironment)
import Data.Functor ((<&>))
import Text.Read (readMaybe)
import Text.Printf (printf)

-- | Should be something like postgresql://[user[:password]@][host][:port][/dbname][?param1=value1&...]
data ConnectionURI = ConnectionURI String

type Env = [(String, String)]
type ErrorMessage = String

buildConnectionURI :: IO (Either String ConnectionURI)
buildConnectionURI = getEnvironment <&> buildConnectionURIWitnEnv

buildConnectionURIWitnEnv :: Env -> Either ErrorMessage ConnectionURI
buildConnectionURIWitnEnv env = do
    host <- lookupEnvWithFallback "PGHOST"
    password <- lookupEnvWithFallback "PGPASSWORD"
    user <- lookupEnvWithFallback "PGUSER"
    database <- lookupEnvWithFallback "PGDATABASE"
    port <- lookupEnvWithFallback "PGPORT"
        >>= (\portStr -> case (readMaybe portStr :: Maybe Int) of
            Nothing -> Left ("Invalid Port" :: ErrorMessage)
            Just n -> Right n
        )
    return $ ConnectionURI
        $ printf "postgresql://%s:%s@%s:%d/%s"
        user password host port database
    where
    lookupEnvWithFallback key = case lookup key env of
        Nothing -> Left $ "Environment variable not found: " ++ key
        Just var -> Right var