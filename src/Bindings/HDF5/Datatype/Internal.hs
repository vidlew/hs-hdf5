{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Bindings.HDF5.Datatype.Internal where

import Data.Int
import Data.Word
import Foreign.C.Types

import Bindings.HDF5.Core
import Bindings.HDF5.Object
import Bindings.HDF5.Raw
import Data.Tagged
import Foreign.Storable

newtype Datatype = Datatype HId_t
    deriving (Eq, HId, FromHId, HDFResultType)

instance Object Datatype where
    staticObjectType = Tagged (Just DatatypeObj)

class Storable t => NativeType t where
    nativeTypeId :: Tagged t Datatype

-- nativeLdouble        = Datatype h5t_NATIVE_LDOUBLE
-- nativeB8             = Datatype h5t_NATIVE_B8
-- nativeB16            = Datatype h5t_NATIVE_B16
-- nativeB32            = Datatype h5t_NATIVE_B32
-- nativeB64            = Datatype h5t_NATIVE_B64
-- nativeOpaque         = Datatype h5t_NATIVE_OPAQUE
-- nativeIntLeast8      = Datatype h5t_NATIVE_INT_LEAST8
-- nativeUintLeast8     = Datatype h5t_NATIVE_UINT_LEAST8
-- nativeIntFast8       = Datatype h5t_NATIVE_INT_FAST8
-- nativeUintFast8      = Datatype h5t_NATIVE_UINT_FAST8
-- nativeIntLeast16     = Datatype h5t_NATIVE_INT_LEAST16
-- nativeUintLeast16    = Datatype h5t_NATIVE_UINT_LEAST16
-- nativeIntFast16      = Datatype h5t_NATIVE_INT_FAST16
-- nativeUintFast16     = Datatype h5t_NATIVE_UINT_FAST16
-- nativeIntLeast32     = Datatype h5t_NATIVE_INT_LEAST32
-- nativeUintLeast32    = Datatype h5t_NATIVE_UINT_LEAST32
-- nativeIntFast32      = Datatype h5t_NATIVE_INT_FAST32
-- nativeUintFast32     = Datatype h5t_NATIVE_UINT_FAST32
-- nativeIntLeast64     = Datatype h5t_NATIVE_INT_LEAST64
-- nativeUintLeast64    = Datatype h5t_NATIVE_UINT_LEAST64
-- nativeIntFast64      = Datatype h5t_NATIVE_INT_FAST64
-- nativeUintFast64     = Datatype h5t_NATIVE_UINT_FAST64

instance NativeType CChar where
    nativeTypeId = Tagged (Datatype h5t_NATIVE_CHAR)

instance NativeType CSChar where
    nativeTypeId = Tagged (Datatype h5t_NATIVE_SCHAR)

instance NativeType CUChar where
    nativeTypeId = Tagged (Datatype h5t_NATIVE_UCHAR)

instance NativeType CShort where
    nativeTypeId = Tagged (Datatype h5t_NATIVE_SHORT)

instance NativeType CUShort where
    nativeTypeId = Tagged (Datatype h5t_NATIVE_USHORT)

instance NativeType CInt where
    nativeTypeId = Tagged (Datatype h5t_NATIVE_INT)

instance NativeType CUInt where
    nativeTypeId = Tagged (Datatype h5t_NATIVE_UINT)

instance NativeType CLong where
    nativeTypeId = Tagged (Datatype h5t_NATIVE_LONG)

instance NativeType CULong where
    nativeTypeId = Tagged (Datatype h5t_NATIVE_ULONG)

instance NativeType CLLong where
    nativeTypeId = Tagged (Datatype h5t_NATIVE_LLONG)

instance NativeType CULLong where
    nativeTypeId = Tagged (Datatype h5t_NATIVE_ULLONG)

instance NativeType CFloat where
    nativeTypeId = Tagged (Datatype h5t_NATIVE_FLOAT)

instance NativeType CDouble where
    nativeTypeId = Tagged (Datatype h5t_NATIVE_DOUBLE)

instance NativeType HAddr where
    nativeTypeId = Tagged (Datatype h5t_NATIVE_HADDR)

instance NativeType HSize where
    nativeTypeId = Tagged (Datatype h5t_NATIVE_HSIZE)

instance NativeType HSSize where
    nativeTypeId = Tagged (Datatype h5t_NATIVE_HSSIZE)

instance NativeType HErr_t where
    nativeTypeId = Tagged (Datatype h5t_NATIVE_HERR)

instance NativeType HBool_t where
    nativeTypeId = Tagged (Datatype h5t_NATIVE_HBOOL)

instance NativeType Int8 where
    nativeTypeId = Tagged (Datatype h5t_NATIVE_INT8)

instance NativeType Int16 where
    nativeTypeId = Tagged (Datatype h5t_NATIVE_INT16)

instance NativeType Int32 where
    nativeTypeId = Tagged (Datatype h5t_NATIVE_INT32)

instance NativeType Int64 where
    nativeTypeId = Tagged (Datatype h5t_NATIVE_INT64)

instance NativeType Word8 where
    nativeTypeId = Tagged (Datatype h5t_NATIVE_UINT8)

instance NativeType Word16 where
    nativeTypeId = Tagged (Datatype h5t_NATIVE_UINT16)

instance NativeType Word32 where
    nativeTypeId = Tagged (Datatype h5t_NATIVE_UINT32)

instance NativeType Word64 where
    nativeTypeId = Tagged (Datatype h5t_NATIVE_UINT64)

if  isIEEE (0 :: Float)
    && floatRadix  (0 :: Float) == 2
    && floatDigits (0 :: Float) == 24
    && floatRange  (0 :: Float) == (-125,128)
    then [d| instance NativeType Float where nativeTypeId = Tagged (Datatype h5t_NATIVE_FLOAT) |]
    else [d| |]

if  isIEEE (0 :: Double)
    && floatRadix  (0 :: Double) == 2
    && floatDigits (0 :: Double) == 53
    && floatRange  (0 :: Double) == (-1021,1024)
    then [d| instance NativeType Double where nativeTypeId = Tagged (Datatype h5t_NATIVE_DOUBLE) |]
    else [d| |]

case sizeOf (0 :: Int) of
    1   -> [d| instance NativeType Int where nativeTypeId = Tagged (Datatype h5t_NATIVE_INT8) |]
    2   -> [d| instance NativeType Int where nativeTypeId = Tagged (Datatype h5t_NATIVE_INT16) |]
    4   -> [d| instance NativeType Int where nativeTypeId = Tagged (Datatype h5t_NATIVE_INT32) |]
    8   -> [d| instance NativeType Int where nativeTypeId = Tagged (Datatype h5t_NATIVE_INT64) |]
    _   -> [d| |]

case sizeOf (0 :: Word) of
    1   -> [d| instance NativeType Word where nativeTypeId = Tagged (Datatype h5t_NATIVE_UINT8) |]
    2   -> [d| instance NativeType Word where nativeTypeId = Tagged (Datatype h5t_NATIVE_UINT16) |]
    4   -> [d| instance NativeType Word where nativeTypeId = Tagged (Datatype h5t_NATIVE_UINT32) |]
    8   -> [d| instance NativeType Word where nativeTypeId = Tagged (Datatype h5t_NATIVE_UINT64) |]
    _   -> [d| |]

nativeTypeOf :: NativeType t => t -> Datatype
nativeTypeOf it = untagAs it nativeTypeId
    where
        untagAs :: t -> Tagged t a -> a
        untagAs _ = untag

nativeTypeOf1 :: NativeType t => f t -> Datatype
nativeTypeOf1 it = untagAs1 it nativeTypeId
    where
        untagAs1 :: f t -> Tagged t a -> a
        untagAs1 _ = untag

hdfTypeOf :: NativeType t => t -> HId_t
hdfTypeOf = hid . nativeTypeOf

hdfTypeOf1 :: NativeType t => f t -> HId_t
hdfTypeOf1 = hid . nativeTypeOf1


data Class
    = Integer
    | Float
    | Time
    | String
    | BitField
    | Opaque
    | Compound
    | Reference
    | Enum
    | VLen
    | Array
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

classCode :: Class -> H5T_class_t
classCode Integer   = h5t_INTEGER
classCode Float     = h5t_FLOAT
classCode Time      = h5t_TIME
classCode String    = h5t_STRING
classCode BitField  = h5t_BITFIELD
classCode Opaque    = h5t_OPAQUE
classCode Compound  = h5t_COMPOUND
classCode Reference = h5t_REFERENCE
classCode Enum      = h5t_ENUM
classCode VLen      = h5t_VLEN
classCode Array     = h5t_ARRAY

classFromCode :: H5T_class_t -> Class
classFromCode c
    | c == h5t_INTEGER      = Integer
    | c == h5t_FLOAT        = Float
    | c == h5t_TIME         = Time
    | c == h5t_STRING       = String
    | c == h5t_BITFIELD     = BitField
    | c == h5t_OPAQUE       = Opaque
    | c == h5t_COMPOUND     = Compound
    | c == h5t_REFERENCE    = Reference
    | c == h5t_ENUM         = Enum
    | c == h5t_VLEN         = VLen
    | c == h5t_ARRAY        = Array
    | otherwise = error ("Unknown H5T_class_t " ++ show c)

data CSet
    = ASCII
    | Reserved2
    | Reserved3
    | Reserved4
    | Reserved5
    | Reserved6
    | Reserved7
    | Reserved8
    | Reserved9
    | Reserved10
    | Reserved11
    | Reserved12
    | Reserved13
    | Reserved14
    | Reserved15
    | UTF8
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

cSetCode :: CSet -> H5T_cset_t
cSetCode ASCII          = h5t_CSET_ASCII
cSetCode Reserved2      = h5t_CSET_RESERVED_2
cSetCode Reserved3      = h5t_CSET_RESERVED_3
cSetCode Reserved4      = h5t_CSET_RESERVED_4
cSetCode Reserved5      = h5t_CSET_RESERVED_5
cSetCode Reserved6      = h5t_CSET_RESERVED_6
cSetCode Reserved7      = h5t_CSET_RESERVED_7
cSetCode Reserved8      = h5t_CSET_RESERVED_8
cSetCode Reserved9      = h5t_CSET_RESERVED_9
cSetCode Reserved10     = h5t_CSET_RESERVED_10
cSetCode Reserved11     = h5t_CSET_RESERVED_11
cSetCode Reserved12     = h5t_CSET_RESERVED_12
cSetCode Reserved13     = h5t_CSET_RESERVED_13
cSetCode Reserved14     = h5t_CSET_RESERVED_14
cSetCode Reserved15     = h5t_CSET_RESERVED_15
cSetCode UTF8           = h5t_CSET_UTF8

cSetFromCode :: H5T_cset_t -> CSet
cSetFromCode c = case lookup c cSets of
    Just cset -> cset
    Nothing   -> error ("Unknown charset code: " ++ show c)
    where cSets = [ (cSetCode x, x) | x <- [minBound .. maxBound]]

data ByteOrder
    = LE
    | BE
    | VAX
    | Mixed
    deriving (Eq, Ord, Bounded, Enum, Read, Show)

byteOrderCode :: Maybe ByteOrder -> H5T_order_t
byteOrderCode (Just LE)     = h5t_ORDER_LE
byteOrderCode (Just BE)     = h5t_ORDER_BE
byteOrderCode (Just VAX)    = h5t_ORDER_VAX
byteOrderCode (Just Mixed)  = h5t_ORDER_MIXED
byteOrderCode Nothing       = h5t_ORDER_NONE

byteOrder :: H5T_order_t -> Maybe ByteOrder
byteOrder c
    | c == h5t_ORDER_LE     = Just LE
    | c == h5t_ORDER_BE     = Just BE
    | c == h5t_ORDER_VAX    = Just VAX
    | c == h5t_ORDER_MIXED  = Just Mixed
    | c == h5t_ORDER_NONE   = Nothing
    | otherwise             = Nothing

data Pad
    = Zero
    | One
    | Background
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

padCode     :: Pad -> H5T_pad_t
padCode Zero        = h5t_PAD_ZERO
padCode One         = h5t_PAD_ONE
padCode Background  = h5t_PAD_BACKGROUND

padFromCode :: H5T_pad_t -> Pad
padFromCode c
    | c == h5t_PAD_ZERO         = Zero
    | c == h5t_PAD_ONE          = One
    | c == h5t_PAD_BACKGROUND   = Background
    | otherwise = error ("Unknown Pad code " ++ show c)

data Normalization
    = Implied
    | MSBSet
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

normalization :: H5T_norm_t -> Maybe Normalization
normalization c
    | c == h5t_NORM_IMPLIED = Just Implied
    | c == h5t_NORM_MSBSET  = Just MSBSet
    | c == h5t_NORM_NONE    = Nothing
    | otherwise = error "Unknown H5T_norm_t value"

data StringPad
    = NullTerm
    | NullPad
    | SpacePad
    | StringPad_Reserved3
    | StringPad_Reserved4
    | StringPad_Reserved5
    | StringPad_Reserved6
    | StringPad_Reserved7
    | StringPad_Reserved8
    | StringPad_Reserved9
    | StringPad_Reserved10
    | StringPad_Reserved11
    | StringPad_Reserved12
    | StringPad_Reserved13
    | StringPad_Reserved14
    | StringPad_Reserved15
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

stringPadCode :: StringPad -> H5T_str_t
stringPadCode NullTerm              = h5t_STR_NULLTERM
stringPadCode NullPad               = h5t_STR_NULLPAD
stringPadCode SpacePad              = h5t_STR_SPACEPAD
stringPadCode StringPad_Reserved3   = h5t_STR_RESERVED_3
stringPadCode StringPad_Reserved4   = h5t_STR_RESERVED_4
stringPadCode StringPad_Reserved5   = h5t_STR_RESERVED_5
stringPadCode StringPad_Reserved6   = h5t_STR_RESERVED_6
stringPadCode StringPad_Reserved7   = h5t_STR_RESERVED_7
stringPadCode StringPad_Reserved8   = h5t_STR_RESERVED_8
stringPadCode StringPad_Reserved9   = h5t_STR_RESERVED_9
stringPadCode StringPad_Reserved10  = h5t_STR_RESERVED_10
stringPadCode StringPad_Reserved11  = h5t_STR_RESERVED_11
stringPadCode StringPad_Reserved12  = h5t_STR_RESERVED_12
stringPadCode StringPad_Reserved13  = h5t_STR_RESERVED_13
stringPadCode StringPad_Reserved14  = h5t_STR_RESERVED_14
stringPadCode StringPad_Reserved15  = h5t_STR_RESERVED_15

stringPadFromCode :: H5T_str_t -> StringPad
stringPadFromCode c
    | c == h5t_STR_NULLTERM     = NullTerm
    | c == h5t_STR_NULLPAD      = NullPad
    | c == h5t_STR_SPACEPAD     = SpacePad
    | c == h5t_STR_RESERVED_3   = StringPad_Reserved3
    | c == h5t_STR_RESERVED_4   = StringPad_Reserved4
    | c == h5t_STR_RESERVED_5   = StringPad_Reserved5
    | c == h5t_STR_RESERVED_6   = StringPad_Reserved6
    | c == h5t_STR_RESERVED_7   = StringPad_Reserved7
    | c == h5t_STR_RESERVED_8   = StringPad_Reserved8
    | c == h5t_STR_RESERVED_9   = StringPad_Reserved9
    | c == h5t_STR_RESERVED_10  = StringPad_Reserved10
    | c == h5t_STR_RESERVED_11  = StringPad_Reserved11
    | c == h5t_STR_RESERVED_12  = StringPad_Reserved12
    | c == h5t_STR_RESERVED_13  = StringPad_Reserved13
    | c == h5t_STR_RESERVED_14  = StringPad_Reserved14
    | c == h5t_STR_RESERVED_15  = StringPad_Reserved15
    | otherwise = error ("Unknown StringPad code " ++ show c)
