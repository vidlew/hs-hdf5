{- |
Typesafe wrappers around HDF5 functions from the H5A API.

Feature coverage is as follows:

  h5a_open
  h5a_close
  h5a_read

-}
module Bindings.HDF5.Attribute
    ( Attribute
    , openAttribute
    , closeAttribute

    , getAttributeSpace
    , readAttribute
    ) where


import qualified Data.ByteString                 as BS
import qualified Data.Vector.Storable            as SV

import           Bindings.HDF5.Core
import           Bindings.HDF5.Dataspace
import           Bindings.HDF5.Datatype.Internal
import           Bindings.HDF5.Error
import           Bindings.HDF5.Raw.H5A
import           Bindings.HDF5.Raw.H5I
import           Bindings.HDF5.Raw.H5P
import           Foreign.Ptr.Conventions

-- * The Attribute type

newtype Attribute = Attribute HId_t
    deriving (Eq, HId, FromHId, HDFResultType)

-- | Open an existing attribute

openAttribute :: Location t =>
                t              -- ^ Parent location
              -> BS.ByteString  -- ^ Attribute name
              -> IO Attribute   -- ^ Resulting attribute
openAttribute loc name =
    Attribute <$> (withErrorCheck $
                   BS.useAsCString name $ \cname ->
                       h5a_open (hid loc) cname h5p_DEFAULT)

-- | Close an Attribute

closeAttribute :: Attribute -> IO ()
closeAttribute (Attribute attr) =
    withErrorCheck_ $
        h5a_close attr

getAttributeSpace :: Attribute -> IO Dataspace
getAttributeSpace (Attribute attr_id) =
    uncheckedFromHId
    <$> withErrorCheck (h5a_get_space attr_id)

-- |Read in data from an attribute
--
-- Parameters:
--
-- [@ attr_id  :: HId_t      @] Attribute to read
--
-- [@ dtype_id :: HId_t      @] Memory datatype of buffer
--
-- [@ buf      :: OutArray a @] Buffer for data to read
--
-- Returns non-negative on success / negative on failure
--
-- This function reads a complete attribute from disk.
--
-- > herr_t  H5Aread(hid_t attr_id, hid_t type_id, void *buf);

readAttribute :: NativeType t =>
                Attribute
              -> IO (SV.Vector t)
readAttribute attr@(Attribute attr_id) = do
  space <- getAttributeSpace attr
  n <- getSimpleDataspaceExtentNPoints space

  withOutVector_ (fromIntegral n) $ \buf ->
    withErrorCheck_ $
      h5a_read attr_id (hdfTypeOf1 buf) buf
