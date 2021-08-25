-- Source: https://github.com/Holmusk/three-layer/blob/master/src/Lib/Core/Id.hs

module Lib.Id.Types (
    Id (..),
    AnyId,
    WithId (..),
    newRandomId,
) where

import Data.Aeson (FromJSON, ToJSON, (.:))
import qualified Data.Aeson as Aeson
import Data.Generics.Labels
import Data.Generics.Product
import Data.Generics.Sum
import Data.HashMap.Strict (insert)
import Data.Proxy
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, symbolVal)

import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import qualified Web.Internal.HttpApiData as Web

newtype Id a = Id {unId :: Text}
    deriving stock (Show, Generic)
    deriving newtype (Eq, Ord, Read, FromJSON, ToJSON, Web.FromHttpApiData)

type AnyId = Id ()

data WithId k a = WithId
    { _id :: !(Id a)
    , content :: !a
    }
    deriving (Eq, Show, Generic, Read)

newRandomId :: IO (Id a)
newRandomId = Id . toText <$> nextRandom

instance (ToJSON a, KnownSymbol k) => ToJSON (WithId k a) where
    toJSON x =
        let val = Aeson.toJSON (content x)
            idVal = Aeson.toJSON (_id x)
            idKey = pack . symbolVal $ Proxy @k
         in case val of
                Aeson.Object obj -> Aeson.Object (insert idKey idVal obj)
                _ -> val

instance (FromJSON a, KnownSymbol k) => FromJSON (WithId k a) where
    parseJSON x = Aeson.withObject "getId" parser x
      where
        parser = \obj -> do
            restObj <- Aeson.parseJSON x
            let idKey = pack . symbolVal $ Proxy @k
            objId <- obj .: idKey
            pure $ WithId objId restObj