module App.Server where

import App.State.Server (StatusAPI, statusServer)
import App.State.StateManager (StateConstraints)
import App.User.Server (UserAPI, userServer)
import Polysemy
import Servant

type AppAPI =
    "status" :> StatusAPI
        :<|> "user" :> UserAPI

appServer :: StateConstraints r => ServerT AppAPI (Sem r)
appServer = statusServer :<|> userServer