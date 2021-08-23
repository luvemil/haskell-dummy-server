module Lib.Polysemy.Storage.Interpreters.InMemory where

import Control.Lens.Operators
import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable)
import Data.IORef
import Data.Maybe (mapMaybe)
import Lib.Polysemy.Storage.Effect
import Polysemy

runStorageWithIORef :: (Member (Embed IO) r, Eq k, Hashable k) => IORef (HM.HashMap k v) -> Sem (Storage k v : r) a -> Sem r a
runStorageWithIORef storeRef = interpret $ \case
    GetByKey k -> do
        store <- embed $ readIORef storeRef
        pure $ HM.lookup k store
    FilterByKey f -> do
        store <- embed $ readIORef storeRef
        let entries =
                mapMaybe
                    (`HM.lookup` store)
                    ( HM.keys store
                        & filter f
                    )
        pure entries
    FilterByValue f -> pure []
    InsertByKey k v -> pure ()
    DeleteByKey k -> pure ()