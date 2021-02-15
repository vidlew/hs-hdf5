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

    , getAttributeInfo
    , getAttributeSpace
    , readAttribute

    , iterateAttributes
    , iterateAttributesByName
    ) where


import           Control.Exception               (SomeException, finally,
                                                  throwIO, try)
import qualified Data.ByteString                 as BS
import           Data.IORef
import qualified Data.Vector.Storable            as SV
import           Foreign
import           Foreign.C

import           Bindings.HDF5.Core
import           Bindings.HDF5.Dataspace
import           Bindings.HDF5.Datatype.Internal
import           Bindings.HDF5.Error
import           Bindings.HDF5.Group
import           Bindings.HDF5.PropertyList.LAPL
import           Bindings.HDF5.Raw.H5
import           Bindings.HDF5.Raw.H5A
import           Bindings.HDF5.Raw.H5I
import           Bindings.HDF5.Raw.H5O
import           Bindings.HDF5.Raw.H5P
import           Bindings.HDF5.Raw.Util
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

data AttributeInfo = AttributeInfo
    { attributeCOrderValid :: Bool
    , attributeCOrder      :: H5O_msg_crt_idx_t
    , attributeCSet        :: CSet
    , attributeDataSize    :: HSize
    } deriving (Eq, Ord, Read, Show)

readAttributeInfo :: H5A_info_t -> AttributeInfo
readAttributeInfo i  = AttributeInfo
    { attributeCOrderValid   = hboolToBool (h5a_info_t'corder_valid i)
    , attributeCOrder        = h5a_info_t'corder i
    , attributeCSet          = cSetFromCode (h5a_info_t'cset i)
    , attributeDataSize      = HSize (h5a_info_t'data_size i)
    }

getAttributeInfo :: Attribute -> IO AttributeInfo
getAttributeInfo (Attribute attr_id) =
    fmap readAttributeInfo $
        withOut_ $ \info ->
            withErrorCheck_ $ h5a_get_info attr_id info

foreign import ccall "wrapper" wrap_H5A_operator2_t
    :: (HId_t -> CString -> In H5A_info_t -> InOut a -> IO HErr_t)
    -> IO (FunPtr (HId_t -> CString -> In H5A_info_t -> InOut a -> IO HErr_t))

with_operator2_t :: (Group -> BS.ByteString -> AttributeInfo -> IO HErr_t)
     -> (H5A_operator2_t () -> InOut () -> IO HErr_t)
     -> IO HErr_t
with_operator2_t op f = do
    exception1 <- newIORef Nothing :: IO (IORef (Maybe SomeException))

    op1 <- wrap_H5A_operator2_t $ \grp name (In attribute) _opData -> do
        name1 <- BS.packCString name
        attribute1 <- peek attribute
        result <- try (op (uncheckedFromHId grp) name1 (readAttributeInfo attribute1))
        case result of
            Left exc -> do
                writeIORef exception1 (Just exc)
                return maxBound
            Right x -> return x

    result <- f op1 (InOut nullPtr) `finally` freeHaskellFunPtr op1

    if result == maxBound
        then do
            exception2 <- readIORef exception1
            maybe (return result) throwIO exception2

        else return result

-- TODO : It would be nice if we didn't expose HErr_t in these callback functions.
--        Decide whether we want Either or Exceptions.
iterateAttributes :: Location t => t -> IndexType -> IterOrder -> Maybe HSize -> (Group -> BS.ByteString -> AttributeInfo -> IO HErr_t) -> IO HSize
iterateAttributes loc indexType order startIndex op =
    fmap HSize $
        withInOut_ (maybe 0 hSize startIndex) $ \ioStartIndex ->
            withErrorCheck_ $
                with_operator2_t op $ \iop opData ->
                    h5a_iterate2 (hid loc) (indexTypeCode indexType) (iterOrderCode order) ioStartIndex iop opData

iterateAttributesByName :: Location t => t -> BS.ByteString -> IndexType -> IterOrder -> Maybe HSize -> Maybe LAPL -> (Group -> BS.ByteString -> AttributeInfo -> IO HErr_t) -> IO HSize
iterateAttributesByName loc groupName indexType order startIndex lapl op =
    fmap HSize $
        withInOut_ (maybe 0 hSize startIndex) $ \ioStartIndex ->
            withErrorCheck_ $
                with_operator2_t op $ \iop opData ->
                    BS.useAsCString groupName $ \cgroupName ->
                        h5a_iterate_by_name (hid loc) cgroupName (indexTypeCode indexType) (iterOrderCode order) ioStartIndex iop opData (maybe h5p_DEFAULT hid lapl)
