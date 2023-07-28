Preamble
-------

> import Diagrams.Runner
> import Physics.Spacetime.Flat (axes)

World line
----------

This is a world line of an object which travels from the origin
faster than the speed of light.

> line :: Diagram B
> line = strokeLine (fromOffsets [V2 (v*t) t]) # lineColor blue

These are the travel time and speed.

> t :: Double
> t = 0.1
>
> v :: Double
> v = 10

Spacetime diagram
-----------------

> main :: IO ()
> main = putDiagram defaultOptions $ line <> axes
