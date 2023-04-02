> module Main (main) where
>
> import Diagrams.Runner
> import Diagrams.Transform.Matrix
> import Linear.Matrix (M22)
>
> main :: IO ()
> main = putDiagram defaultOptions $ hcat [axes, deformedAxes # lc red]

> deformedAxes :: Diagram B
> deformedAxes = axes # transform (fromMat22 (matrix 0.5) (V2 0 0 :: V2 Double))

> axes :: Diagram B
> axes = xAxis <> tAxis <> lightCone

> axisLength :: Double
> axisLength = 2.0

> xAxis :: Diagram B
> xAxis = (-axisLength ^& 0) ~~ (axisLength ^& 0)

> tAxis :: Diagram B
> tAxis = (0 ^& (-axisLength)) `arrowBetween` (0 ^& axisLength)

> lightCone :: Diagram B
> lightCone = (forwardEdge <> backwardEdge) # opacity 0.75
>                                           # dashingN  [0.01, 0.02] 0
>                                           # lw ultraThin
>   where
>    forwardEdge = lowerLeftCorner ~~ upperRightCorner
>    backwardEdge = reflectX forwardEdge
>    lowerLeftCorner = (-axisLength) ^& (-axisLength)
>    upperRightCorner = axisLength ^& axisLength

> matrix :: Double -> M22 Double
> matrix v = V2 (V2 gamma (-gamma * v)) (V2 (-gamma*v) gamma)
>   where
>     gamma = 1 / sqrt (1 - v*v)
