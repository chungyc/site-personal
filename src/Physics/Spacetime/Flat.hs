-- |
-- Description: Manipulate coordinates in flat spacetime.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
--
-- Defines coordinates for flat spacetime and
-- provides functions to manipulate and transform them.
-- It is intended to be a simple module sufficient enough
-- to assist in drawing spacetime diagrams.
module Physics.Spacetime.Flat
  ( -- * Coordinates
    Coordinate (..),

    -- * Simple operations
    plus,
    minus,
    scale,
    combine,
    approximates,

    -- * Spacetime interval
    interval,

    -- * Lorentz transformation
    transform,

    -- * Working with Diagrams
    vectorize2D,
  )
where

import Diagrams.TwoD.Types
import GHC.Generics (Generic)

-- | The coordinates \((t, x, y, z)\) in flat spacetime.
--
-- The coordinates use Planck units.  In other words, \(c = 1\).
newtype Coordinate = Coordinate (Double, Double, Double, Double)
  deriving (Eq, Show, Generic)

-- | Treat two coordinates like vectors and add them together.
--
-- >>> Coordinate (2, 1, 0, 0) `plus` Coordinate (-1, 1, 0, 0)
-- Coordinate (1.0,2.0,0.0,0.0)
plus :: Coordinate -> Coordinate -> Coordinate
plus = combine (+)

-- | Treat two coordinates like vectors and subtract the second one from the first.
--
-- >>> Coordinate (2, 1, 0, 0) `minus` Coordinate (-1, 1, 0, 0)
-- Coordinate (3.0,0.0,0.0,0.0)
minus :: Coordinate -> Coordinate -> Coordinate
minus = combine (-)

-- | Scale each component of a coordinate with the given function.
--
-- >>> scale (*2) $ Coordinate (1, 2, 0, 0)
-- Coordinate (2.0,4.0,0.0,0.0)
scale :: (Double -> Double) -> Coordinate -> Coordinate
scale f (Coordinate (t, x, y, z)) = Coordinate (f t, f x, f y, f z)

-- | Combine the corresponding components of two coordinates with the given function.
--
-- >>> combine (*) (Coordinate (2, 3, 1, 2)) (Coordinate (1, -1, 2, 3))
-- Coordinate (2.0,-3.0,2.0,6.0)
combine :: (Double -> Double -> Double) -> Coordinate -> Coordinate -> Coordinate
combine f (Coordinate (t, x, y, z)) (Coordinate (t', x', y', z')) =
  Coordinate (f t t', f x x', f y y', f z z')

-- | Returns whether two coordinates are approximately equal to each other.
--
-- >>> Coordinate (1, 1, 0, 0) `approximates` Coordinate (1, 1, 0, 0)
-- True
-- >>> Coordinate (1, -1, 0, 0) `approximates` Coordinate (1, 1, 0, 0)
-- False
--
-- The threshold for being approximately equivalent is somewhat arbitrary.
approximates :: Coordinate -> Coordinate -> Bool
approximates (Coordinate (t, x, y, z)) (Coordinate (t', x', y', z')) =
  t `closeTo` t' && x `closeTo` x' && y `closeTo` y' && z `closeTo` z'
  where
    closeTo a b = abs (a - b) < 1e-5

-- | The spacetime interval between the origin and the given coordinates.
--
-- This uses the convention where \(ds^2 = dt^2 - dx^2 - dy^2 - dz^2\).
--
-- >>> interval $ Coordinate (2, 1, 0, 0)
-- 3.0
interval :: Coordinate -> Double
interval (Coordinate (t, x, y, z)) = t ** 2 - x ** 2 - y ** 2 - z ** 2

-- | Apply the Lorentz transformation.
--
-- This transforms coordinates in an original inertial frame to those
-- in a new inertial frame.  It is assumed that the origin coincides
-- between the two frames, and that the velocity only has an \(x\) component.
--
-- >>> let expected = Coordinate (0.414213562,0.414213562,2.0,3.0)
-- >>> let actual = transform (sqrt 0.5) $ Coordinate (1, 1, 2, 3)
-- >>> actual `approximates` expected
-- True
--
-- For reference, look up the
-- [Lorentz transformation](https://chungyc.org/article/reference/physics/relativity/#lorentz).
transform ::
  -- | Velocity of the new frame in the original frame.
  Double ->
  -- | Coordinate in the original frame.
  Coordinate ->
  -- | Coordinate in the new frame.
  Coordinate
transform v (Coordinate (t, x, y, z)) = Coordinate (t', x', y', z')
  where
    gamma = 1 / sqrt (1 - v ** 2)
    t' = gamma * (t - v * x)
    x' = gamma * (x - v * t)
    y' = y
    z' = z

-- | Convert given coordinates into a two-dimensional "Diagrams" vector.
--
-- It will project the \(x\) coordinate to the \(x\) component
-- and the \(t\) coordinate to the \(y\) component of the vector.
-- The other coordinates will be ignored.
--
-- >>> vectorize2D $ Coordinate (1, 1, 0, 0)
-- V2 1.0 1.0
vectorize2D :: Coordinate -> V2 Double
vectorize2D (Coordinate (t, x, _, _)) = r2 (x, t)
