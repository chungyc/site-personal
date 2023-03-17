import Diagrams.TwoD.Path.LSystem

main = putDiagram defaultOptions example

example :: Diagram B
example = getTurtleDiagram $ sierpinski 8
