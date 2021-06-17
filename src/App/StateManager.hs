{-# LANGUAGE TemplateHaskell #-}

module App.StateManager where

import App.State
import Control.Lens.Operators
import Polysemy
import Polysemy.AtomicState
import Polysemy.Trace

data StateManager r a where
    Start :: StateManager r AppState
    Stop :: StateManager r AppState

makeSem ''StateManager

type StateConstraints r = (Member Trace r, Member (AtomicState AppState) r, Member (Embed IO) r)

turnOn :: (Member Trace r, Member (AtomicState AppState) r, Member (Embed IO) r) => Sem r AppState
turnOn = do
    trace "Called turnOn"
    sOrig <- atomicGet
    case sOrig ^. #asRunStatus of
        (AppOn _) -> do
            trace "Already ON, nothing to do"
            pure sOrig
        AppOff -> do
            trace "Found app OFF, turning ON"
            s <- embed $ turnAppOn sOrig
            atomicPut s
            pure s

turnOff :: (Member Trace r, Member (AtomicState AppState) r) => Sem r AppState
turnOff = do
    trace "Called turnOff"
    s <- atomicGet
    let newState = s & #asRunStatus .~ AppOff
    atomicPut newState
    pure newState

showState :: (Member Trace r, Member (AtomicState AppState) r) => Sem r AppState
showState = trace "Called showState" >> atomicGet
