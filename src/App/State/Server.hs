module App.State.Server where

import App.State.State
import App.State.StateManager
import Polysemy
import Servant

type StatusAPI =
    Get '[JSON] AppState
        :<|> "start" :> Post '[JSON] AppState
        :<|> "stop" :> Post '[JSON] AppState

statusServer :: StateConstraints r => ServerT StatusAPI (Sem r)
statusServer = showState :<|> turnOn :<|> turnOff