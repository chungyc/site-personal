\subsection{Preamble}

> import Diagrams.Runner
> import Physics.Spacetime.Flat (axes)

\subsection{World lines}

There are two world lines.

One is for the twin which stays on Earth.

> earthTwin :: Diagram B
> earthTwin = strokeLine (fromOffsets [r2 (0, 2*t)])
>             # lineColor blue
>             # lineWidth veryThick
>             # translateY (-t)

The other is for the twin who travels to a star and back.

> travelingTwin :: Diagram B
> travelingTwin = strokeLine (fromOffsets [r2 (v * t, t), r2 ((-v) * t, t)])
>                 # lineColor red
>                 # lineWidth veryThick
>                 # translateY (-t)

The twin on the Earth experiences time \(2t\) passing,
\(t\) for the traveling twin to arrive at the star,
and \(t\) more for the trip back.

> t :: Double
> t = 0.9

The traveling twin is traveling at the following speed:

> v :: Double
> v = 0.75

\subsection{Spacetime diagram}

> main :: IO ()
> main = putDiagram defaultOptions $ earthTwin <> travelingTwin <> axes
