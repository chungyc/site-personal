import Diagrams.TwoD.Path.IteratedSubset

main = putDiagram defaultOptions example

example :: Diagram B
example = strokeTrail $ snowflake 5
