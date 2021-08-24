{-# LANGUAGE TemplateHaskell #-}

module Lib.Polysemy.Storage.Effect where

import Control.Monad (forM_)
import Polysemy

data Storage k v r a where
    GetByKey :: k -> Storage k v r (Maybe v)
    FilterByKey :: (k -> Bool) -> Storage k v r [v]
    FilterByValue :: (v -> Bool) -> Storage k v r [v]
    InsertByKey :: k -> v -> Storage k v r ()
    DeleteByKey :: k -> Storage k v r ()

makeSem ''Storage

getAllValues :: Member (Storage k v) r => Sem r [v]
getAllValues = filterByKey $ const True

insertMany :: Member (Storage k v) r => [(k, v)] -> Sem r ()
insertMany es = forM_ es $ uncurry insertByKey