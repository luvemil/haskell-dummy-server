module App.User.Server where

import App.User.User
import Lib.Resource.Server (ResouceAPI)
import Polysemy
import Servant

type UserAPI = ResouceAPI UserResource User UserId "userId"

userServer :: ServerT UserAPI (Sem r)
userServer = undefined