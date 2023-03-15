myCircle :: Diagram B
myCircle = circle 0.8 # fc blue

main = putStr $ prettyText $ renderDia SVG defaultOptions $ pad 1.1 $ myCircle
