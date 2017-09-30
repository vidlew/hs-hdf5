module Spec.File ( describeFile ) where

import Test.Hspec
-- import Test.QuickCheck

import qualified Data.ByteString.Char8 as BS
import qualified Bindings.HDF5.File as F
import qualified Data.Vector.Storable as SV

import Spec.Util

-- | Describe the H5F File interface
describeFile :: Spec
describeFile = do
  it "isHDF5 returns True for the test file" $ do
    isHdf5 <- withTestFile $ \path file -> do
      -- Flish file to disk
      F.flushFile file F.Global
      -- Return whether it is a valid HDF5 file
      F.isHDF5 (BS.pack path)
    isHdf5 `shouldBe` True

  it "getFileName reports correct filename" $ do
    (fn, path) <- withTestFile $ \path file -> do
      fn <- F.getFileName file
      return (fn, BS.pack path)
    fn `shouldBe` path

    {- TODO : create a group and mount it there so we can test something about it
    it "Allows mounting of one file in another" $ do
      x <- withTestFile $ \path file1 -> let callback file2 = do
                                               F.mountFile file1 (BS.pack "/") file2 Nothing
                                               return 42
                                         in withTestFile' callback
      x `shouldBe` 42
    -}


  it "global object count is zero initially" $ do
    count <- F.getFileObjCount Nothing True [F.All]
    count `shouldBe` 0

  around withTestFile' $ do
    it "test file has one file" $ \file -> do
      count <- F.getFileObjCount (Just file) True [F.Files]
      count `shouldBe` 1

    it "test file has no groups" $ \file -> do
      count <- F.getFileObjCount (Just file) True [F.Groups]
      count `shouldBe` 0

    it "test file has no datasets" $ \file -> do
      count <- F.getFileObjCount (Just file) True [F.Datasets]
      count `shouldBe` 0

    it "test file has 1 open object" $ \file -> do
      vec <- F.getOpenObjects (Just file)True [F.All]
      SV.length vec `shouldBe` 1

    it "test file has no free space" $ \file -> do
      size <- F.getFileFreespace file
      size `shouldBe` 0
