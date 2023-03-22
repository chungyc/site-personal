import Diagrams.Runner
import Diagrams.TwoD.Path.LSystem

main :: IO ()
main = putDiagram defaultOptions example

example :: Diagram B
example = getTurtleDiagram $ sierpinski 8
