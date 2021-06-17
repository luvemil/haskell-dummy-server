module App.State where

import App.Config
import Control.Lens.Operators ((&), (.~), (^.))
import Data.Aeson
import Data.Generics.Labels
import Data.Generics.Product
import Data.Generics.Sum

import Data.Text
import Data.Time
import GHC.Generics

data AppRunStatus = AppOn UTCTime | AppOff
    deriving (Eq, Show, Generic)

data AppState = AppState
    { asRunStatus :: AppRunStatus
    , asBuild :: String
    }
    deriving (Eq, Show, Generic)

instance ToJSON AppState where
    toJSON (AppState AppOff build) = object ["status" .= ("OFF" :: Text), "build" .= build]
    toJSON (AppState (AppOn d) build) =
        object
            [ "status" .= ("ON" :: Text)
            , "start_time" .= toJSON d
            , "build" .= build
            ]

newOnApp :: Config -> IO AppState
newOnApp c = do
    time <- getCurrentTime
    pure $ AppState (AppOn time) (c ^. #configBuildVersion)

turnAppOn :: AppState -> IO AppState
turnAppOn appState =
    case appState ^. #asRunStatus of
        AppOn _ -> pure appState
        AppOff -> do
            time <- getCurrentTime
            pure $ appState & #asRunStatus .~ AppOn time
