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
module Physics.Spacetime.Flat (Coordinate (..), interval, transform) where

-- | The coordinates \((t, x, y, z)\) in flat spacetime.
--
-- The coordinates use Planck units.  In other words, \(c = 1\).
--
-- This is not of the 'Eq' type class because one should usually not
-- compare floating-point values for exact equality.
newtype Coordinate = Coordinate (Double, Double, Double, Double)
  deriving (Show)

-- | The spacetime interval between the origin and the given coordinates.
--
-- This uses the convention where \(ds^2 = dt^2 - dx^2 - dy^2 - dz^2\).
interval :: Coordinate -> Double
interval (Coordinate (t, x, y, z)) = t ** 2 - x ** 2 - y ** 2 - z ** 2

-- | Apply the Lorentz transformation.
--
-- This transforms coordinates in an original intertial frame to those
-- in a new inertial frame.  It is assumed that the origin coincides
-- between the two frames, and that the velocity only has an \(x\) component.
--
-- For reference, look up the
-- [Lorentz transformation](https://chungyc.org/article/physics/relativity/#lorentz).
transform ::
  -- | Velocity of the new frame in the original frame.
  Double ->
  -- | Coordinate in the original frame.
  Coordinate ->
  -- | Coordinate in the new frame.
  Coordinate
transform v (Coordinate (t, x, y, z)) = Coordinate (t', x', y', z')
  where
    gamma = 1 - v ** 2
    t' = gamma * (t - v * x)
    x' = gamma * (x - v * t)
    y' = y
    z' = z
