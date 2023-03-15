{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Text.Lazy.IO (putStr)
import Diagrams.Backend.SVG
import Diagrams.Prelude
import Graphics.Svg
import Prelude hiding (putStr)

myCircle :: Diagram B
myCircle = circle 1

main = putStr $ prettyText $ renderDia SVG options myCircle
  where
    options = SVGOptions (mkWidth 250) Nothing "" [] True
