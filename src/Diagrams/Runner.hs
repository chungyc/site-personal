{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- Description: Exports convenience function for rendering Diagrams in Hakyll.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Diagrams.Runner
  ( defaultOptions,
    putDiagram,
    module Diagrams.Prelude,
    module Diagrams.Backend.SVG,
  )
where

import Data.Text.Lazy.IO (putStr)
import Diagrams.Backend.SVG
import Diagrams.Prelude
import Graphics.Svg
import Prelude hiding (putStr)

putDiagram :: (SVGFloat n) => Options SVG V2 n -> QDiagram SVG V2 n Any -> IO ()
putDiagram options diagram = putStr $ prettyText $ renderDia SVG options $ pad 1.1 $ diagram

defaultOptions :: (Num n) => Options SVG V2 n
defaultOptions = SVGOptions (mkWidth 400) Nothing "" [] True
