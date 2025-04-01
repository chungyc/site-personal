-- |
-- Description: Tests for "Data.Numbered".
-- Copyright: Copyright (C) 2025 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Data.NumberedSpec (spec) where

import Data.Numbered
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  let xs = build id :: Numbered Integer
   in prop "values are consistent with their numbering" $ \(Positive x) ->
        find xs x `shouldBe` x

  prop "values are built from numbering" $ \(Positive n) (Fn f) ->
    find (build f :: Numbered String) n `shouldBe` f (n :: Int)
