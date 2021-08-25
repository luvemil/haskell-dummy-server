module Lib.Resource.ResourceManager where

import Control.Lens.Operators
import Lib.Id (Id, WithId (WithId))
import Lib.Id.Effects
import Lib.Polysemy.Storage
import Polysemy
import Polysemy.Error
import Servant (NoContent (NoContent))

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

resourceUpdateExisting ::
    ( Member (Storage (Id b) (WithId k b)) r
    , Member (Error String) r
    ) =>
    WithId k b ->
    Sem r (WithId k b)
resourceUpdateExisting res = do
    let resId = res ^. #_id
    curRes <- getByKey resId
    case curRes of
        Nothing -> throw "Not found"
        Just _ -> do
            insertByKey resId res
            getByKey resId >>= \case
                Nothing -> throw "Error"
                Just x -> pure x

resourceGetOne ::
    ( Member (Storage (Id b) (WithId k b)) r
    ) =>
    Id b ->
    Sem r (Maybe (WithId k b))
resourceGetOne = getByKey

resourceDeleteOne ::
    ( Member (Storage (Id b) (WithId k b)) r
    ) =>
    Id b ->
    Sem r NoContent
resourceDeleteOne k = deleteByKey k >> pure NoContent