module Lib (
    main,
) where

import App.Config
import App.Runner
import App.State.State
import Control.Lens
import Data.IORef
import Data.Maybe
import Network.Wai.Handler.Warp (run)
import System.Environment
import Text.Read

main :: IO ()
main = do
    config <- loadConfig
    app <- createApp config
    run (config ^. #configPort) app