module Lib.Enrich where

import Data.Aeson (FromJSON, ToJSON, (.:))
import qualified Data.Aeson as Aeson
import Data.HashMap.Strict (insert)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Web.Internal.HttpApiData as Web

data Enrich a b = Enrich
    { aData :: !a
    , bData :: !b
    }
    deriving (Show, Eq, Generic, Read)

instance (ToJSON a, ToJSON b) => ToJSON (Enrich a b) where
    toJSON x =
        let aVal = Aeson.toJSON (aData x)
            bVal = Aeson.toJSON (bData x)
         in undefined