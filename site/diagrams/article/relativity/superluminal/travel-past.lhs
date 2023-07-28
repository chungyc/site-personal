Preamble
-------

> import Diagrams.Runner
> import Physics.Spacetime.Flat (axes, transformation)

World line
----------

This is a world line of an object which travels from the origin
faster than the speed of light.

> line :: Diagram B
> line = strokeLine worldline # lineColor blue
>   where
>     outbound = fromOffsets [V2 (v*t) t]
>     outbound' = outbound # transform (transformation v')
>     inbound = outbound' <> fromOffsets [V2 (-v*t) t]
>     worldline = inbound # transform (transformation (-v'))

These are the travel time and speed.

> t :: Double
> t = 0.1
>
> v :: Double
> v = 10

The other reference frame has this velocity relative to the original reference frame.

> v' :: Double
> v' = 0.9

Spacetime diagram
-----------------

> main :: IO ()
> main = putDiagram defaultOptions $ line <> axes
