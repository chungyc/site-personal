{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Description: Tests for "Physics.Spacetime.Flat".
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Physics.Spacetime.FlatSpec (spec) where

import Diagrams.Prelude (V2 (..))
import Physics.Spacetime.Flat
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (scale)

spec :: Spec
spec = do
  prop "combines coordinates" $
    \c@(Coordinate (t, x, y, z)) c'@(Coordinate (t', x', y', z')) func ->
      let f = applyFun2 func
       in combine f c c' `shouldBe` Coordinate (f t t', f x x', f y y', f z z')

  prop "scales coordinates" $
    \c@(Coordinate (t, x, y, z)) func ->
      let f = applyFun func
       in scale f c `shouldBe` Coordinate (f t, f x, f y, f z)

  prop "adds coordinates" $ \c c' ->
    c `plus` c' `shouldSatisfy` approximates (combine (+) c c')

  prop "subtracts coordinates" $ \c c' ->
    c `minus` c' `shouldSatisfy` approximates (combine (-) c c')

  prop "approximately equals" $ \c@(Coordinate (t, x, y, z)) ->
    -- We take advantage of the fact that we know the threshold to set the range.
    forAll (choose (-1e-7, 1e-7)) $ \dt ->
      forAll (choose (-1e-7, 1e-7)) $ \dx ->
        forAll (choose (-1e-7, 1e-7)) $ \dy ->
          forAll (choose (-1e-7, 1e-7)) $ \dz ->
            c `approximates` Coordinate (t + dt, x + dx, y + dy, z + dz) `shouldBe` True

  prop "does not approximately equal" $ \c@(Coordinate (t, x, y, z)) ->
    -- We take advantage of the fact that we know the threshold to set the range.
    forAll (arbitrary `suchThat` ((<) 1e-4 . abs)) $ \dt ->
      forAll (arbitrary `suchThat` ((<) 1e-4 . abs)) $ \dx ->
        forAll (arbitrary `suchThat` ((<) 1e-4 . abs)) $ \dy ->
          forAll (arbitrary `suchThat` ((<) 1e-4 . abs)) $ \dz ->
            c `approximates` Coordinate (t + dt, x + dx, y + dy, z + dz) `shouldNotBe` True

  prop "computes spacetime interval" $ \c@(Coordinate (t, x, y, z)) ->
    interval c `shouldSatisfy` closeTo (t * t - x * x - y * y - z * z)

  prop "spacetime interval invariant under transform" $ \c ->
    forAll (choose (0, 1) `suchThat` (< 1)) $ \v ->
      transform v c `shouldSatisfy` closeTo (interval c) . interval

  prop "turns into two-dimensional vector" $ \c@(Coordinate (t, x, _, _)) ->
    vectorize2D c `shouldBe` V2 x t

instance Arbitrary Coordinate where
  arbitrary = Coordinate <$> arbitrary
  shrink = genericShrink

-- | Checks whether two floating-point values are approximately equal.
--
-- The treshold for approximate equivalence is somewhat arbitrary.
closeTo :: Double -> Double -> Bool
closeTo x y = abs (x - y) < 1e-5
