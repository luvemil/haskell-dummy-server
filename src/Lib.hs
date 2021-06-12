module Lib (
    main,
) where

import App.Runner
import App.State
import Data.IORef
import Data.Maybe
import Network.Wai.Handler.Warp (run)
import System.Environment
import Text.Read

main :: IO ()
main = do
    p <- lookupEnv "PORT"
    let port :: Int = fromMaybe 5000 (p >>= readMaybe)
    initialStateRef <- newOnApp >>= newIORef
    let config = Config initialStateRef
    run port $ createApp config