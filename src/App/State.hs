module App.State where

import Data.Aeson
import Data.Text
import Data.Time

data AppRunStatus = AppOn UTCTime | AppOff
    deriving (Eq, Show)

data AppState = AppState
    { asRunStatus :: AppRunStatus
    }
    deriving (Eq, Show)

instance ToJSON AppState where
    toJSON (AppState AppOff) = object ["status" .= ("OFF" :: Text)]
    toJSON (AppState (AppOn d)) =
        object
            [ "status" .= ("ON" :: Text)
            , "start_time" .= toJSON d
            ]

newOnApp :: IO AppState
newOnApp = do
    AppState . AppOn <$> getCurrentTime