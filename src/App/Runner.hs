module App.Runner where

import App.Config
import App.Server
import App.State.State
import App.User.User (User, UserId)
import Control.Lens.Operators
import Control.Monad.Except
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Lazy as HM
import Data.IORef
import Data.Proxy
import Lib.Id.Effects (runGenIDToIO)
import Lib.Polysemy.Storage (runStorageWithIORef)
import Polysemy
import Polysemy.AtomicState
import Polysemy.Error (runError)
import Polysemy.Trace
import Servant.Server

createApp :: Config -> IO Application
createApp config = do
  appOn <- newOnApp config
  appStateRef <- newIORef appOn
  userStorageRef <- newIORef HM.empty
  pure $ serve (Proxy @AppAPI) (liftServer appStateRef userStorageRef)

liftServer :: IORef AppState -> IORef (HM.HashMap UserId User) -> ServerT AppAPI Handler
liftServer appStateRef userStorageRef = hoistServer (Proxy @AppAPI) (interpretServer appStateRef userStorageRef) appServer
 where
  interpretServer asRef usRef sem =
    sem
      & runAtomicStateIORef asRef
      & runStorageWithIORef usRef
      & runGenIDToIO
      & traceToIO
      & runError @String
      & runM
      & liftToHandler
  liftToHandler = Handler . ExceptT . fmap handleError
  handleError :: Either String a -> Either ServerError a
  handleError (Left message) = Left err400{errBody = BS.pack message}
  handleError (Right x) = Right x