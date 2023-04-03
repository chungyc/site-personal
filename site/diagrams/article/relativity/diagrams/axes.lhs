> module Main (main) where
>
> import Diagrams.Runner
> import Diagrams.Transform.Matrix
> import Physics.Spacetime.Flat qualified as Spacetime
>
> main :: IO ()
> main = putDiagram defaultOptions $
>   hcat [ axes
>        , deformedAxes <> axes # opacity 0.25 # lc red]

> deformedAxes :: Diagram B
> deformedAxes = axes # transform (Spacetime.transformation 0.5)

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
