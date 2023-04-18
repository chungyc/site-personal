> module Main (main) where
>
> import Diagrams.Runner
> import Physics.Spacetime.Flat qualified as Spacetime
>
> main :: IO ()
> main = putDiagram defaultOptions $
>   hcat [ axes
>        , deformedAxes <> axes # opacity 0.25 # lc red]

> deformedAxes :: Diagram B
> deformedAxes = axes # transform (Spacetime.transformation 0.5)

> axes :: Diagram B
> axes = Spacetime.axes
