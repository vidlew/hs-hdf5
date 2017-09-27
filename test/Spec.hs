import Test.Hspec
import Test.QuickCheck
import System.IO.Temp
import System.IO
import Control.Exception (bracket)

import qualified Data.ByteString.Char8 as BS
import qualified Bindings.HDF5.File as F

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
