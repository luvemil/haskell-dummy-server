{-# LANGUAGE TemplateHaskell #-}

module Lib.Polysemy.Storage.Effect where

import Polysemy

data Storage k v r a where
    GetByKey :: k -> Storage k v r (Maybe v)
    FilterByKey :: (k -> Bool) -> Storage k v r [v]
    FilterByValue :: (v -> Bool) -> Storage k v r [v]
    InsertByKey :: k -> v -> Storage k v r ()
    DeleteByKey :: k -> Storage k v r ()

makeSem ''Storage