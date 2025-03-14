\subsection{Preamble}

> import Diagrams.Runner
> import Physics.Spacetime.Flat (axes, transformation)

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

\subsection{\(x\) axes for traveling twin}

This is the \(x\) axis in a stationary inertial frame.

> axis :: Diagram B
> axis = ((-1) ^& 0) ~~ (1 ^& 0)

This is the \(x\) axis for the inertial frame on the outbound trip
for the traveling twin in the Earth's inertial frame.

> outboundAxis :: Diagram B
> outboundAxis = axis # transform (transformation v) # translate (r2 (v*t, 0))

This is the \(x\) axis for the inertial frame on the inbound trip
for the traveling twin in the Earth's inertial frame.

> inboundAxis :: Diagram B
> inboundAxis = axis # transform (transformation (-v)) # translate (r2 (v*t, 0))

\subsection{Spacetime diagram}

> main :: IO ()
> main = putDiagram defaultOptions $ mconcat
>   [ earthTwin
>   , travelingTwin
>   , outboundAxis
>   , inboundAxis
>   , axes # opacity 0.1
>   ]
