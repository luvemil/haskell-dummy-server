module App.User.Server where

import App.User.User
import Lib.Resource.ResourceManager (FullConstraint)
import Lib.Resource.Server (ResourceAPIWithId, resourceServer)
import Polysemy
import Servant

type UserAPI = ResourceAPIWithId UserResource "id" "userId"

type UserServerConstraint r = FullConstraint UserResource "id" r

userServer :: UserServerConstraint r => ServerT UserAPI (Sem r)
userServer = resourceServer