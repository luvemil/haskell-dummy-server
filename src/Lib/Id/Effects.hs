{-# LANGUAGE TemplateHaskell #-}

module Lib.Id.Effects where

import Lib.Id.Types
import Polysemy

data GenID r a where
    NewId :: GenID r (Id t)

makeSem ''GenID

runGenIDToIO :: Member (Embed IO) r => Sem (GenID : r) a -> Sem r a
runGenIDToIO = interpret $ \case
    NewId -> embed newRandomId