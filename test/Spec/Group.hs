{-# LANGUAGE OverloadedStrings #-}

module Spec.Group ( describeGroup ) where

import           Test.Hspec

import           Bindings.HDF5.Core    (IndexType (..), IterOrder (..), hid)
import qualified Bindings.HDF5.Group   as G
import qualified Bindings.HDF5.Link    as L
import qualified Bindings.HDF5.Object  as OB
import qualified Data.ByteString.Char8 as BS

import           Spec.Util

-- | Describe the H5G Group interface
describeGroup :: Spec
describeGroup = do
  around (withGroup "group1") $ do
    it "can create group at top-level" $ \file -> do
      name <- L.getLinkNameByIdx file "/" ByName Increasing 0 Nothing
      name `shouldBe` "group1"

  -- TODO : test lookup non-existent group
