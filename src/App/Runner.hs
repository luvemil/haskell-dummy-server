module App.Runner where

import App.Config
import App.Server
import App.State
import Control.Lens.Operators
import Control.Monad.Except
import Data.IORef
import Data.Proxy
import Polysemy
import Polysemy.AtomicState
import Polysemy.Trace
import Servant.Server

createApp :: Config -> IO Application
createApp config = do
  appOn <- newOnApp config
  appStateRef <- newIORef $ appOn
  pure $ serve (Proxy @StatusAPI) (liftServer appStateRef)

liftServer :: IORef AppState -> ServerT StatusAPI Handler
liftServer appStateRef = hoistServer (Proxy @StatusAPI) (interpretServer appStateRef) statusServer
 where
  interpretServer asRef sem =
    sem
      & runAtomicStateIORef asRef
      & traceToIO
      & runM
      & liftToHandler
  liftToHandler = Handler . ExceptT . fmap Right