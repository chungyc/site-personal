main = putDiagram defaultOptions example

circles :: Diagram B
circles = hcat $ map circle [1..6]

example :: Diagram B
example = vcat $ replicate 3 circles