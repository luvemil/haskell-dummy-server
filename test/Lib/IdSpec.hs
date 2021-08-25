module Lib.IdSpec where

import Control.Lens.Operators
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Lib.Id
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

data DummyData = DummyData
    { dummyDataInt :: Int
    , dummyDataString :: String
    }
    deriving (Generic, Show, Eq)

instance Aeson.FromJSON DummyData
instance Aeson.ToJSON DummyData

type DummyDataId = Id DummyData
type IdKey = "qwerty"
type MyData = WithId IdKey DummyData

mockJSONData :: BS.ByteString
mockJSONData = "{\"qwerty\": \"asdf\", \"dummyDataInt\": 23, \"dummyDataString\": \"text\"}"

mockData :: MyData
mockData = WithId myId myData
  where
    myId = Id "asdf"
    myData = DummyData 23 "text"

fromText :: String -> b -> WithId IdKey b
fromText = WithId . (Id . pack)

mockDataDecode :: BS.ByteString -> Either String MyData
mockDataDecode = Aeson.eitherDecode

spec :: Spec
{-# NOINLINE spec #-}
spec = describe "Checks on DummyData" $ do
    it "has the correct data" $
        property $
            \(k, i, s) -> fromText k (DummyData i s) `shouldSatisfy` \x -> x ^. #content == DummyData i s
    it "has the correct key" $
        property $
            \(k, i, s) -> fromText k (DummyData i s) `shouldSatisfy` \x -> x ^. #_id == Id (pack k)
    it "has the correct FromJSON instance" $
        decodedJSON `shouldBe` Right mockData
    it "has the correct ToJSON instance" $
        (mockDataDecode =<< reencodedJSON) `shouldBe` Right mockData
  where
    decodedJSON = mockDataDecode mockJSONData
    reencodedJSON = Aeson.encode <$> decodedJSON