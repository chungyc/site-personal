main = putDiagram defaultOptions example

example :: Diagram B
example = hrule (2 * sum sizes) === circles # centerX
  where
    circles = hcat . map alignT . zipWith scale sizes $ repeat $ circle 1
    sizes = [2, 5, 4, 7, 1, 3]
