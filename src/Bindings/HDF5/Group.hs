{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
Typesafe wrappers around HDF5 functions from the H5G API.

Feature coverage is as follows:

  h5g_get_info                  	[  OK  ]
  h5g_get_objname_by_idx        	[ FAIL ] (deprecated)
  h5g_get_objinfo               	[ FAIL ] (deprecated)
  h5g_iterate                   	[ FAIL ] (deprecated)
  h5g_get_info_by_idx           	[ FAIL ]
  h5g_link                      	[ FAIL ] (deprecated)
  h5g_unlink                    	[ FAIL ] (deprecated)
  h5g_get_objtype_by_idx        	[ FAIL ] (deprecated)
  h5g_get_linkval               	[ FAIL ] (deprecated)
  h5g_create_anon               	[  OK  ]
  h5g_get_info_by_name          	[  OK  ]
  h5g_get_num_objs              	[ FAIL ] (deprecated)
  h5g_close                     	[  OK  ]
  h5g_move                      	[ FAIL ] (deprecated)
  h5g_open1                     	[ FAIL ] (deprecated)
  h5g_open2                     	[  OK  ]
  h5g_link2                     	[ FAIL ] (deprecated)
  h5g_set_comment               	[ FAIL ] (deprecated)
  h5g_get_comment               	[ FAIL ] (deprecated)
  h5g_get_create_plist          	[ FAIL ]
  h5g_move2                     	[ FAIL ] (deprecated)
  h5g_create2                   	[  OK  ]
  h5g_create1                   	[ FAIL ] (deprecated)


-}
module Bindings.HDF5.Group
    ( Group

    , createGroup
    , createAnonymousGroup
    , openGroup
    , closeGroup

    , GroupStorageType(..)
    , GroupInfo(..)

    , getGroupInfo
    , getGroupInfoByName
    ) where

import Bindings.HDF5.Core
import Bindings.HDF5.Error
import Bindings.HDF5.Object
import Bindings.HDF5.PropertyList.GAPL
import Bindings.HDF5.PropertyList.GCPL
import Bindings.HDF5.PropertyList.LCPL
import Bindings.HDF5.Raw.H5G
import Bindings.HDF5.Raw.H5I
import Bindings.HDF5.Raw.H5P
import Bindings.HDF5.Raw.Util
import qualified Data.ByteString as BS
import Data.Int
import Foreign.Ptr.Conventions

-- * The Group type

newtype Group = Group HId_t
    deriving (Eq, HId, FromHId, HDFResultType)

instance Location Group
instance Object Group where
    staticObjectType = Tagged (Just GroupObj)

-- * General group functions

-- | Create a group given name, location and properties

createGroup :: Location t =>
               t                 -- ^ Parent location for the group
               -> BS.ByteString  -- ^ Group name
               -> Maybe LCPL     -- ^ Link creation properties
               -> Maybe GCPL     -- ^ Group creation properties
               -> Maybe GAPL     -- ^ Group access properties
               -> IO Group       -- ^ Resulting group
createGroup loc name lcpl gcpl gapl =
    fmap Group $
        withErrorCheck $
            BS.useAsCString name $ \cname ->
                h5g_create2 (hid loc) cname
                    (maybe h5p_DEFAULT hid lcpl)
                    (maybe h5p_DEFAULT hid gcpl)
                    (maybe h5p_DEFAULT hid gapl)

-- | Create an anonymous group without a name
createAnonymousGroup :: Location t =>
                        t              -- ^ Parent location for the group
                        -> Maybe GCPL  -- ^ Group creation properties
                        -> Maybe GAPL  -- ^ Group access properties
                        -> IO Group    -- ^ Resulting group
createAnonymousGroup loc gcpl gapl =
    fmap Group $
        withErrorCheck $
            h5g_create_anon (hid loc) (maybe h5p_DEFAULT hid gcpl) (maybe h5p_DEFAULT hid gapl)

-- | Open an existing group
openGroup :: Location t =>
             t                 -- ^ Parent location
             -> BS.ByteString  -- ^ Group name
             -> Maybe GAPL     -- ^ Group access properties
             -> IO Group       -- ^ Resulting group
openGroup loc name gapl =
    fmap Group $
        withErrorCheck $
            BS.useAsCString name $ \cname ->
                h5g_open2 (hid loc) cname (maybe h5p_DEFAULT hid gapl)

-- | Close a group
closeGroup :: Group -> IO ()
closeGroup (Group grp) =
    withErrorCheck_ $
        h5g_close grp

-- * Group metadata

data GroupStorageType
    = CompactStorage
    | DenseStorage
    | SymbolTableStorage
    | UnknownStorage
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

groupStorageTypeFromCode :: H5G_storage_type_t -> GroupStorageType
groupStorageTypeFromCode c
    | c == h5g_STORAGE_TYPE_COMPACT         = CompactStorage
    | c == h5g_STORAGE_TYPE_DENSE           = DenseStorage
    | c == h5g_STORAGE_TYPE_SYMBOL_TABLE    = SymbolTableStorage
    | otherwise                             = UnknownStorage

data GroupInfo = GroupInfo
    { groupStorageType  :: !GroupStorageType
    , groupNLinks       :: !HSize
    , groupMaxCOrder    :: !Int64
    , groupMounted      :: !Bool
    } deriving (Eq, Ord, Read, Show)

readGroupInfo :: H5G_info_t -> GroupInfo
readGroupInfo (H5G_info_t a b c d) = GroupInfo (groupStorageTypeFromCode a) (HSize b) c (hboolToBool d)

getGroupInfo :: Group -> IO GroupInfo
getGroupInfo (Group group_id) =
    fmap readGroupInfo $
        withOut_ $ \info ->
            withErrorCheck_ $
                h5g_get_info group_id info

getGroupInfoByName :: Location loc => loc -> BS.ByteString -> Maybe LAPL -> IO GroupInfo
getGroupInfoByName loc name lapl =
    fmap readGroupInfo $
        withOut_ $ \info ->
            BS.useAsCString name $ \cname ->
                withErrorCheck_ $
                    h5g_get_info_by_name (hid loc) cname info (maybe h5p_DEFAULT hid lapl)
