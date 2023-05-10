\subsection{Preamble}

> import Diagrams.Runner
> import Physics.Spacetime.Flat (axes)

\subsection{World lines}

There are two world lines.

> travelingTwin :: Diagram B
> travelingTwin = strokeLine (fromOffsets [r2 (v * t, t), r2 (-v * t, t)])
>                 # lineColor red
>                 # lineWidth veryThick
>                 # translateY (-t)

> travelingTwin' :: Diagram B
> travelingTwin' = strokeLine (fromOffsets [r2 (-v * t, t), r2 (v * t, t)])
>                 # lineColor blue
>                 # lineWidth veryThick
>                 # translateY (-t)

On Earth, time \(t\) passes, \(t\) for the twins to arrive at their stars,
and \(t\) more for the trips back.

> t :: Double
> t = 0.9

The traveling twins is traveling at the following speed relative to Earth:

> v :: Double
> v = 0.75

\subsection{Spacetime diagram}

> main :: IO ()
> main = putDiagram defaultOptions $ travelingTwin <> travelingTwin' <> axes
