-- |
-- Description: Tests for "Mathematics.Collatz".
-- Copyright: Copyright (C) 2025 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Mathematics.CollatzSpec (spec) where

import Data.List (genericLength)
import Mathematics.Collatz
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "next" $ do
    prop "even number n is followed by n `div` 2" $ \(Positive m) ->
      let n = 2 * m
       in next n `shouldBe` n `div` 2

    prop "odd number n is followed by 3*n+1" $ \(NonNegative m) ->
      let n = 2 * m + 1
       in next n `shouldBe` 3 * n + 1

  describe "collatz" $ do
    prop "sequences start with the number" $ \(Positive n) ->
      head (collatz n) `shouldBe` n

    prop "sequences end in 1" $ \(Positive n) ->
      last (collatz n) `shouldBe` 1

    prop "starting from 1 ends in 1" $
      collatz 1 `shouldBe` [1]

    prop "number should be followed by rest of sequence" $ \(Positive m) ->
      let n = m + 1
       in collatz n `shouldBe` n : collatz (next n)

  describe "collatzLength" $ do
    prop "equal to length of sequence" $ \(Positive n) ->
      collatzLength n `shouldBe` genericLength (collatz n)

  describe "collatzMax" $ do
    prop "equal to maximum element in sequence" $ \(Positive n) ->
      collatzMax n `shouldBe` maximum (collatz n)
