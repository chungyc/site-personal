myCircle :: Diagram B
myCircle = circle 0.8

main = putStr $ prettyText $ renderDia SVG options myCircle
  where
    options = SVGOptions (mkWidth 250) Nothing "" [] True
