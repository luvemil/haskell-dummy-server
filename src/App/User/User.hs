module App.User.User where

import Data.Aeson
import Data.Generics.Labels
import Data.Generics.Product
import Data.Generics.Sum
import Data.Text (Text)
import GHC.Generics
import Lib.Id

data UserResource = UserResource
    { userUserName :: Text
    , userEmail :: Text
    }
    deriving (Eq, Show, Generic, Read)

instance FromJSON UserResource
instance ToJSON UserResource

type User = WithId "id" UserResource
