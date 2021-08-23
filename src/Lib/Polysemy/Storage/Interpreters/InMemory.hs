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
    FilterByValue f -> do
        store <- embed $ readIORef storeRef
        let entries =
                store
                    & HM.toList
                    & map snd
                    & filter f
        pure entries
    InsertByKey k v -> do
        store <- embed $ readIORef storeRef
        let newStore = HM.insert k v store
        embed $ writeIORef storeRef newStore
        pure ()
    DeleteByKey k -> do
        store <- embed $ readIORef storeRef
        let newStore = HM.delete k store
        embed $ writeIORef storeRef newStore
        pure ()