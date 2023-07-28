Preamble
-------

> import Diagrams.Runner
> import Physics.Spacetime.Flat (axes, transformation)

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

World line from another reference frame
---------------------------------------

This is the same world line as view from another reference frame.

> line' :: Diagram B
> line' = line # transform (transformation v')

The reference frame has this velocity relative to the original reference frame.

> v' :: Double
> v' = 0.9

Spacetime diagram
-----------------

> main :: IO ()
> main = putDiagram defaultOptions $ line' <> axes
