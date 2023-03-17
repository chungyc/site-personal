import Diagrams.TwoD.Path.LSystem

main = putDiagram defaultOptions example

example :: Diagram B
example = rotateBy 0.25 $ getTurtleDiagram $ tree4 7
