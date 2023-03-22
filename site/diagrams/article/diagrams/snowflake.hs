import Diagrams.Runner
import Diagrams.TwoD.Path.IteratedSubset

main :: IO ()
main = putDiagram defaultOptions example

example :: Diagram B
example = strokeTrail $ snowflake 5
