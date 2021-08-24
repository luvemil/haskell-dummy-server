module Lib.Resource.ResourceManager where

import Control.Lens.Operators
import Lib.Id (Id, WithId (WithId))
import Lib.Id.Effects
import Lib.Polysemy.Storage
import Polysemy
import Polysemy.Error

type FullConstraint b k r =
    ( Member (Storage (Id b) (WithId k b)) r
    , Member GenID r
    , Member (Error String) r
    )

resourceGetAll :: Member (Storage k v) r => Sem r [v]
resourceGetAll = getAllValues

resourceCreateNew ::
    ( Member (Storage (Id b) (WithId k b)) r
    , Member GenID r
    , Member (Error String) r
    ) =>
    b ->
    Sem r (WithId k b)
resourceCreateNew resource = do
    uuid <- newId
    let newResource = WithId uuid resource
    insertByKey uuid newResource
    res <- getByKey uuid
    case res of
        Just x -> pure x
        Nothing -> throw "Error"

-- resourceUpdateExisting ::
--     ( Member (Storage (Id b) (WithId k b)) r
--     , Member (Error String) r
--     ) =>
--     WithId k b ->
--     Sem r (WithId k b)
-- resourceUpdateExisting res = do
--     let resId = res ^. #_id
--     curRes <- getByKey resId
--     pure undefined