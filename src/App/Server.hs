module App.Server where

import App.State
import App.StateManager
import Polysemy
import Servant

type StatusAPI =
    "status" :> Get '[JSON] AppState
        :<|> "start" :> Post '[JSON] AppState
        :<|> "stop" :> Post '[JSON] AppState

statusServer :: StateConstraints r => ServerT StatusAPI (Sem r)
statusServer = showState :<|> turnOn :<|> turnOff