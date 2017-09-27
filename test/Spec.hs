import Test.Hspec
import Test.QuickCheck
import System.IO.Temp
import System.IO
import Control.Exception (bracket)

import qualified Data.ByteString.Char8 as BS
import qualified Bindings.HDF5.File as F
import qualified Data.Vector.Storable as SV


main :: IO ()
main = hspec $ do
  describe "File interface" $ do
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

    it "test file has one file" $ do
      count <- withTestFile' $ \file -> F.getFileObjCount (Just file) True [F.Files]
      count `shouldBe` 1

    it "test file has no groups" $ do
      count <- withTestFile' $ \file -> F.getFileObjCount (Just file) True [F.Groups]
      count `shouldBe` 0

    it "test file has no datasets" $ do
      count <- withTestFile' $ \file -> F.getFileObjCount (Just file) True [F.Datasets]
      count `shouldBe` 0

    it "test file has 1 open object" $ do
      vec <- withTestFile' $ \file -> F.getOpenObjects (Just file)True [F.All]
      SV.length vec `shouldBe` 1

    it "test file has no free space" $ do
      size <- withTestFile' $ \file -> F.getFileFreespace file
      size `shouldBe` 0

-- | Call a handle with a temporary HDF5 file
withTestFile :: (FilePath -> F.File -> IO a) -> IO a
withTestFile callback = withSystemTempFile "hs-hdf5.hdf" callback2 where
  callback2 path handle = bracket aquire release run where
    aquire = do
      hClose handle
      F.createFile (BS.pack path) [F.Truncate] Nothing Nothing
    run file = do callback path file
    release file = do F.closeFile file

withTestFile' :: (F.File -> IO a) -> IO a
withTestFile' callback = withTestFile (\path file -> callback file)
