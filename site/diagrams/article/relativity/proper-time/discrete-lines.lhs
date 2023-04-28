\subsection{Preamble}

> import Diagrams.Runner
> import Physics.Spacetime.Flat (axesWith, axesOptions, axesLength)
> import Physics.Spacetime.Flat qualified as Spacetime

\subsection{World line}

This world line is a set of connected straight world lines.
Used for illustrating how the total proper time can be obtained
by adding up the proper times for the individual segments.

> worldline :: Diagram B
> worldline =
>   strokeLine (fromOffsets offsets)
>       # translate (r2 (-0.15, -2))
>       # lineColor blue
>   where
>     offsets = [ V2 0.2 0.5
>               , V2 (-0.35) 0.5
>               , V2 0.4 0.5
>               , V2 (-0.1) 0.5
>               , V2 0.3 0.5
>               , V2 0 0.5
>               , V2 0.4 0.5
>               , V2 0 0.5
>               ]

\subsection{Axes}

> axes = axesWith axesOptions { axesLength = 2 }

\subsection{The diagram}

> main = putDiagram defaultOptions $ worldline <> axes
