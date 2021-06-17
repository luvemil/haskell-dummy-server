module App.Config where

import Data.Generics.Labels
import Data.Generics.Product
import Data.Generics.Sum
import Data.Maybe
import GHC.Generics
import System.Environment
import Text.Read

-- | App Configuration as read from Env/Options/Config files
data Config = Config
    { configPort :: Int
    , configBuildVersion :: String
    }
    deriving (Eq, Show, Generic)

loadConfig :: IO Config
loadConfig = do
    p <- lookupEnv "PORT"
    let port :: Int = fromMaybe 5000 (p >>= readMaybe)
    b <- lookupEnv "BUILD_VERSION"
    let buildVersion = fromMaybe "null" (b >>= readMaybe)
    pure $ Config port buildVersion