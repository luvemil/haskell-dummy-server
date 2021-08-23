module App.User.Server where

import App.User.User
import Polysemy
import Servant

type UserAPI =
    Get '[JSON] [User]
        :<|> ReqBody '[JSON] UserResource :> Post '[JSON] User
        :<|> ReqBody '[JSON] User :> Put '[JSON] User
        :<|> Capture "userId" UserId :> Get '[JSON] User
        :<|> Capture "userId" UserId :> Delete '[] NoContent

userServer :: ServerT UserAPI (Sem r)
userServer = undefined