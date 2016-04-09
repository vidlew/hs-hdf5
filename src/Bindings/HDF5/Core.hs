{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bindings.HDF5.Core
    ( module Bindings.HDF5.Core
    , module Bindings.HDF5.Core.HId
    , module Bindings.HDF5.Core.HDFResultType
    ) where

import Bindings.HDF5.Raw.H5

import Bindings.HDF5.Core.HId
import Bindings.HDF5.Core.HDFResultType

import Data.Bits
import Foreign.Storable

newtype HSize = HSize HSize_t
    deriving (Eq, Ord, Enum, Bounded, Num, Real, Integral, Bits, Storable)

hSize :: HSize -> HSize_t
hSize (HSize s) = s

instance Read HSize where
    readsPrec p s =
        [ (HSize (HSize_t n), rest)
        | (n,rest) <- readsPrec p s
        ]

instance Show HSize where
    showsPrec p (HSize (HSize_t n)) = showsPrec p n

newtype HSSize = HSSize HSSize_t
    deriving (Eq, Ord, Enum, Bounded, Num, Real, Integral, Bits, Storable)

hSSize :: HSSize -> HSSize_t
hSSize (HSSize s) = s

instance Read HSSize where
    readsPrec p s =
        [ (HSSize (HSSize_t n), rest)
        | (n,rest) <- readsPrec p s
        ]

instance Show HSSize where
    showsPrec p (HSSize (HSSize_t n)) = showsPrec p n

newtype HAddr = HAddr HAddr_t
    deriving (Eq, Ord, Enum, Bounded, Num, Real, Integral, Bits, Storable)

hAddr :: HAddr -> HAddr_t
hAddr (HAddr a) = a

instance Read HAddr where
    readsPrec p s =
        [ (HAddr (HAddr_t n), rest)
        | (n, rest) <- readsPrec p s
        ]

instance Show HAddr where
    showsPrec p (HAddr (HAddr_t n)) = showsPrec p n

data IH_Info = IH_Info
    { indexSize     :: !HSize
    , heapSize      :: !HSize
    } deriving (Eq, Ord, Read, Show)

data IndexType
    = ByName
    | ByCreationOrder
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

indexTypeCode :: IndexType -> H5_index_t
indexTypeCode ByName            = h5_INDEX_NAME
indexTypeCode ByCreationOrder   = h5_INDEX_CRT_ORDER

data IterOrder
    = Increasing
    | Decreasing
    | Native
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

iterOrderCode :: IterOrder -> H5_iter_order_t
iterOrderCode Increasing = h5_ITER_INC
iterOrderCode Decreasing = h5_ITER_DEC
iterOrderCode Native     = h5_ITER_NATIVE
