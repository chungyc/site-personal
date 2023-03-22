import Diagrams.Runner

main :: IO ()
main = putDiagram defaultOptions example

example :: Diagram B
example = hrule (2 * sum sizes) === circles # centerX
  where
    circles = hcat $ map (alignT . (`scale` circle 1)) sizes
    sizes = [2, 5, 4, 7, 1, 3]
