import Diagrams.Runner

main :: IO ()
main = putDiagram defaultOptions myCircle

myCircle :: Diagram B
myCircle = circle 1 # fc red
