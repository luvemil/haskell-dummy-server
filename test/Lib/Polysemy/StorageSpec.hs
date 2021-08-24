module Lib.Polysemy.StorageSpec where

import Control.Lens.Operators
import Control.Monad (forM_)
import qualified Data.HashMap.Lazy as HM
import Data.List (nubBy)
import qualified Data.Set as Set
import Lib.Polysemy.Storage
import Polysemy
import Test.Hspec
import Test.QuickCheck
import UnliftIO.IORef (newIORef)

main :: IO ()
main = hspec spec

type PersistentStore = Storage String Int

initializeDB :: Member PersistentStore r => Sem r ()
initializeDB = do
  forM_ [1 .. 10] $ \x -> insertByKey (show x) x

interpretFull :: Sem '[PersistentStore, Embed IO] a -> IO a
interpretFull a = do
  s <- newIORef (HM.empty @String @Int)
  a
    & runStorageWithIORef s
    & runM

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

spec :: Spec
{-# NOINLINE spec #-}
spec = describe "Storage IORef Interpretation" $ do
  it "get existing key" $
    interpretFull (getSomeKey "2") `shouldReturn` Just 2
  it "can't find a non existing key" $
    interpretFull (getSomeKey "notfound") `shouldReturn` Nothing
  it "Get after insert returns the value put inside" $
    property $
      \(k, v) -> interpretFull (insertAndGet k v) `shouldReturn` Just v
  it "Removing a key works" $
    property $
      \(k, v) -> interpretFull (insertAndRemove k v) `shouldReturn` Nothing
  it "Inserts all elements" $
    property $
      \es ->
        interpretFull
          ( insertManyElems es >>= \x -> pure $ Set.fromList x
          )
          `shouldReturn` ( reverse es
                            & nubBy (curry $ uncurry (==) . both fst)
                            & map snd
                            & Set.fromList
                         )
 where
  getSomeKey :: Member PersistentStore r => String -> Sem r (Maybe Int)
  getSomeKey k = initializeDB >> getByKey k
  insertAndRemove :: Member PersistentStore r => String -> Int -> Sem r (Maybe Int)
  insertAndRemove k v = insertByKey k v >> deleteByKey k >> getByKey k
  insertAndGet :: Member PersistentStore r => String -> Int -> Sem r (Maybe Int)
  insertAndGet k v = insertByKey k v >> getByKey k
  insertManyElems :: Member PersistentStore r => [(String, Int)] -> Sem r [Int]
  insertManyElems es = do
    insertMany es
    getAllValues