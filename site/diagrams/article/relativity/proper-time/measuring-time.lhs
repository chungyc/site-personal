\subsection{Preamble}

> import Diagrams.Runner
> import Physics.Spacetime.Flat (axesWith, axesOptions, axesLength)
> import Physics.Spacetime.Flat qualified as Spacetime

\subsection{World line}

This is a random smooth world line to raise the following question:
How much time does an observer on the world line experience?

> worldline :: Diagram B
> worldline = strokeLine (fromOffsets [V2 1 2]) # lineColor blue # lineWidth veryThick

> observerFrame :: Diagram B
> observerFrame = worldline <> axes

\subsection{In the stationary frame}

> stationaryFrame :: Diagram B
> stationaryFrame = observedFrame <> stationaryAxes
>   where
>     observedFrame = observerFrame # transform (Spacetime.transformation 0.5)
>     stationaryAxes = axes # lineColor red # opacity 0.5

\subsection{Axes}

> axes = axesWith axesOptions { axesLength = 2 }

\subsection{The diagram}

> main = putDiagram defaultOptions $ hcat' (with & sep .~ 0.5) [observerFrame, stationaryFrame]
