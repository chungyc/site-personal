\subsection{Preamble}

> import Diagrams.Runner
> import Physics.Spacetime.Flat (axes)

\subsection{World lines}

There are two world lines.

One is for the twin which stays on Earth.

> earthTwin :: Diagram B
> earthTwin = strokeLine (fromOffsets [r2 (0, 1.8)])
>             # lineColor blue
>             # lineWidth veryThick
>             # translateY (-0.9)

The other is for the twin who travels to a star and back.

> travelingTwin :: Diagram B
> travelingTwin = strokeLine (fromOffsets [r2 (0.89, 0.9), r2 (-0.89, 0.9)])
>                 # lineColor red
>                 # lineWidth veryThick
>                 # translateY (-0.9)

\subsection{Spacetime diagram}

> main = putDiagram defaultOptions $ earthTwin <> travelingTwin <> axes
