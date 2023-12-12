module Database.Connection (buildConnectionURI) where
import Text.Printf (printf)
import System.Environment.MrEnv (envAsInt, envAsString)

-- | Should be something like postgresql://[user[:password]@][host][:port][/dbname][?param1=value1&...]
data ConnectionURI = ConnectionURI String

buildConnectionURI :: IO ConnectionURI
buildConnectionURI = do
    port <- envAsInt "POSTGRES_PORT" 5432
    host <- envAsString "POSTGRES_HOST" "localhost"
    database <- envAsString "POSTGRES_DB" "postgres"
    user <- envAsString "POSTGRES_USER" "postgres"
    password <- envAsString "POSTGRES_PASSWORD" "password"
    return $ ConnectionURI
        $ printf "postgresql://%s:%s@%s:%d/%s"
        user password host port database
