module Lib.Resource.Server where

import Lib.Id
import Lib.Resource.ResourceManager
import Polysemy
import Servant

type ResouceAPI b f i ik =
    Get '[JSON] [f]
        :<|> ReqBody '[JSON] b :> Post '[JSON] f
        :<|> ReqBody '[JSON] f :> Put '[JSON] f
        :<|> Capture ik i :> Get '[JSON] (Maybe f)
        :<|> Capture ik i :> DeleteNoContent

type ResourceAPIWithId b i ik = ResouceAPI b (WithId i b) i ik

resourceServer :: FullConstraint b i r => ServerT (ResourceAPIWithId b i ik) (Sem r)
resourceServer = resourceGetAll :<|> resourceCreateNew :<|> undefined

-- resourceServer :: ServerT (ResourceAPIWithId b ik) (Sem r)
-- resourceServer = undefined