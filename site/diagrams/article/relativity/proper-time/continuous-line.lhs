\subsection{Preamble}

> import Diagrams.Runner
> import Physics.Spacetime.Flat (axesWith, axesOptions, axesLength)
> import Physics.Spacetime.Flat qualified as Spacetime

\subsection{World line}

> worldline :: Diagram B
> worldline =
>   strokeLine (cubicSpline False points)
>       # translate (r2 (-0.15, -2))
>       # lineColor blue
>   where
>     points = map p2 [ (0, 0)
>                     , (0.2, 0.5)
>                     , (-0.15, 1.0)
>                     , (0.25, 1.5)
>                     , (0.15, 2.0)
>                     , (0.45, 2.5)
>                     , (0.45, 3.0)
>                     , (0.85, 3.5)
>                     , (0.85, 4.0)
>                     ]

\subsection{Axes}

> axes = axesWith axesOptions { axesLength = 2 }

\subsection{The diagram}

> main = putDiagram defaultOptions $ worldline <> axes
