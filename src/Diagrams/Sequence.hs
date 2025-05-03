{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- Description: Plotting numerical sequences.
-- Copyright: Copyright (C) 2025 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Diagrams.Sequence (plot) where

import Diagrams.Backend.Rasterific
import Diagrams.Prelude
import Diagrams.Transform.ScaleInv

single :: (Integral c) => c -> c -> Diagram B
single n x =
  --  fromVertices [p2 (n' - 0.5, x'), p2 (n' + 0.5, x')]
  (scaleInv (circle 0.01) unitY ^. scaleInvObj)
    # translate (n' ^& x')
    # fillColor black
    # lineWidth (normalized 0.01)
  where
    n' = fromIntegral n
    x' = fromIntegral x

plot :: (Integral c) => [c] -> Diagram B
plot xs = transform s plot'
  where
    plot' = mconcat $ zipWith single [1 ..] xs
    s = scalingX (aspectRatio / width plot') <> scalingY (1 / height plot')

aspectRatio :: Double
aspectRatio = 1.6
