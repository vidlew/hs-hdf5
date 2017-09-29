module Spec.Util (
  withTestFile
  , withTestFile'
  ) where

import System.IO.Temp
import System.IO
import Control.Exception (bracket)

import qualified Data.ByteString.Char8 as BS
import qualified Bindings.HDF5.File as F

-- TODO : Use H5FD_CORE driver to avoid writing files : will require high-level wrapper for H5Pset_fapl_core

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
