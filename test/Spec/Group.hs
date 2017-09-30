{-# LANGUAGE OverloadedStrings #-}

module Spec.Group ( describeGroup ) where

import Test.Hspec
-- import Test.QuickCheck

import qualified Data.ByteString.Char8 as BS
import qualified Bindings.HDF5.Group as G
import qualified Bindings.HDF5.Object as OB
import Bindings.HDF5.Core (hid)

import Spec.Util

-- | Describe the H5G Group interface
describeGroup :: Spec
describeGroup = do
  around withGroup "group1" $ \file -> do
    it "can create group at top-level" $ do
      -- TODO : find group once get_objname_idx is implemented
      group <- undefined
      return (hid group1, hid group2)
    hid1 `shouldBe` hid2

  -- TODO : test lookup non-existent group
