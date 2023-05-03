\subsection{Preamble}

> import Diagrams.Runner
> import Physics.Spacetime.Flat (axesWith, axesOptions, axesLength)

\subsection{World lines}

There are two world lines.

One stays stationary.

> stationaryLine :: Diagram B
> stationaryLine = strokeLine (fromOffsets [r2 (0,1.5)]) # lineColor blue # lineWidth veryThick

The other meanders around before reaching the same event.

> meanderingLine :: Diagram B
> meanderingLine = strokeLine (cubicSpline False points) # lineColor red # lineWidth veryThick
>   where
>     points = map p2 [(0, 0), (0.2, 0.4), (0, 1), (-0.15, 1.3), (0, 1.5)]

\subsection{Axes}

> axes = axesWith axesOptions { axesLength = 2 }

\subsection{The diagram}

> main = putDiagram defaultOptions $ stationaryLine <> meanderingLine <> axes
