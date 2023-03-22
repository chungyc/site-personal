import Diagrams.Runner

main :: IO ()
main = putDiagram defaultOptions example

example :: Diagram B
example = circle 2 # lc purple `atop` circle 1 # lc green
