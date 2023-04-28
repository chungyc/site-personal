\subsection{Preamble}

> import Diagrams.Runner
> import Physics.Spacetime.Flat (axesWith, axesOptions, axesLength)

\subsection{World line}

This is a random smooth world line to raise the following question:
How much time does an observer on the world line experience?

> worldline :: Diagram B
> worldline =
>   strokeLine (cubicSpline False points)
>       # translate (r2 (1, -2))
>       # lineColor blue
>   where
>     points = map p2 [(1, -2), (0.1, -1), (0, 0), (0.35, 1), (0.2, 2)]

\subsection{Axes}

> axes = axesWith axesOptions { axesLength = 2 }

\subsection{The diagram}

> main = putDiagram defaultOptions $ worldline <> axes
