\subsection{Preamble}

> import Diagrams.Runner
> import Physics.Spacetime.Flat (axes, transformation)

\subsection{World lines}

There are two world lines.

> travelingTwin :: Diagram B
> travelingTwin = strokeLine (fromOffsets [r2 (v * t, t), r2 ((-v) * t, t)])
>                 # lineColor red
>                 # lineWidth veryThick
>                 # translateY (-t)

> travelingTwin' :: Diagram B
> travelingTwin' = strokeLine (fromOffsets [r2 ((-v) * t, t), r2 (v * t, t)])
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

\subsection{\(x\) axes for traveling twin}

This is the \(x\) axis in a stationary inertial frame.

> axis :: Diagram B
> axis = ((-1) ^& 0) ~~ (1 ^& 0)

These are is the \(x\) axes for the inertial frame on the outbound trips.

> outboundAxis :: Diagram B
> outboundAxis = axis # transform (transformation v) # translate (r2 (v*t, 0))
>
> outboundAxis' :: Diagram B
> outboundAxis' = axis # transform (transformation (-v)) # translate (r2 ((-v)*t, 0))

These are the \(x\) axes for the inertial frame on the inbound trips.

> inboundAxis :: Diagram B
> inboundAxis = axis # transform (transformation (-v)) # translate (r2 (v*t, 0))
>
> inboundAxis' :: Diagram B
> inboundAxis' = axis # transform (transformation v) # translate (r2 ((-v)*t, 0))

\subsection{Spacetime diagram}

> main :: IO ()
> main = putDiagram defaultOptions $ mconcat
>   [ travelingTwin
>   , travelingTwin'
>   , outboundAxis
>   , inboundAxis
>   , outboundAxis'
>   , inboundAxis'
>   , axes # opacity 0.25
>   ]
