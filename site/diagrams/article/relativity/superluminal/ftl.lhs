Preamble
-------

> import Diagrams.Runner
> import Physics.Spacetime.Flat (axes)

World line
----------

> line :: Diagram B
> line = strokeLine (fromOffsets [V2 (v*t) t]) # lineColor blue

> t :: Double
> t = 0.1

> v :: Double
> v = 10

Spacetime diagram
-----------------

> main :: IO ()
> main = putDiagram defaultOptions $ line <> axes
