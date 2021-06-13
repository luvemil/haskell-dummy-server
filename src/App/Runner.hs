module App.Runner where

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

data Config = Config
  { cInitialState :: IORef AppState
  }

createApp :: Config -> Application
createApp config = serve (Proxy @StatusAPI) (liftServer config)

liftServer :: Config -> ServerT StatusAPI Handler
liftServer config = hoistServer (Proxy @StatusAPI) (interpretServer config) statusServer
 where
  interpretServer c sem =
    sem
      & runAtomicStateIORef (cInitialState c)
      & traceToIO
      & runM
      & liftToHandler
  liftToHandler = Handler . ExceptT . fmap Right