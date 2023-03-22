import Diagrams.Runner

main :: IO ()
main = putDiagram defaultOptions example

example :: Diagram B
example = circle 1 === square 2
