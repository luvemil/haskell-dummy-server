{-# LANGUAGE TemplateHaskell #-}

module App.StateManager where

import App.State
import Polysemy
import Polysemy.AtomicState

data StateManager r a where
    Start :: StateManager r AppState
    Stop :: StateManager r AppState

makeSem ''StateManager

type StateConstraints r = (Member (AtomicState AppState) r, Member (Embed IO) r)

turnOn :: (Member (AtomicState AppState) r, Member (Embed IO) r) => Sem r AppState
turnOn = do
    s <- embed newOnApp
    atomicPut s
    pure s

turnOff :: (Member (AtomicState AppState) r) => Sem r AppState
turnOff = do
    let s = AppState AppOff
    atomicPut s
    pure s

showState :: (Member (AtomicState AppState) r) => Sem r AppState
showState = atomicGet
