module App.User.Server where

import App.User.User
import Polysemy
import Servant

type UserAPI =
    Get '[JSON] [User]
        :<|> ReqBody '[JSON] UserResource :> Post '[JSON] User

userServer :: ServerT UserAPI (Sem r)
userServer = undefined