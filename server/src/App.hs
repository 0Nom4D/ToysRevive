module App (State(..), AppM) where

import Database.PostgreSQL.Simple
import Control.Monad.Trans.Reader (ReaderT)
import Servant (Handler)

data State = State
    { dbConnection :: Connection
    }

type AppM = ReaderT State Handler