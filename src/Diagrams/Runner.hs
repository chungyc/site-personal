{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- Description: Exports convenience function for rendering Diagrams in Hakyll.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
--
-- Exports the 'putDiagram' function for conveniently printing SVG generated from a diagram.
--
-- This also re-exports the "Diagrams.Prelude" and "Diagrams.Backend.SVG" modules,
-- so that code does not have to import these separately.
module Diagrams.Runner
  ( putDiagram,
    defaultOptions,
    module Diagrams.Prelude,
    module Diagrams.Backend.SVG,
  )
where

import Data.Text.Lazy.IO (putStr)
import Diagrams.Backend.SVG
import Diagrams.Prelude
import Graphics.Svg
import Prelude hiding (putStr)

-- |
-- Write out SVG for the given diagram to standard output.
--
-- >>> putDiagram defaultOptions $ circle 1
-- <?xml version="1.0" encoding="UTF-8"?>
-- ...
--
-- It can be passed in a fully specified 'SVGOptions',
-- but it is usually enough to give the default set of options in 'defaultOptions'.
putDiagram :: (SVGFloat n) => Options SVG V2 n -> QDiagram SVG V2 n Any -> IO ()
putDiagram options diagram =
  putStr $
    renderText $
      renderDia SVG options $
        -- Include padding to prevent non-zero width lines from being cut off.
        frame 1.1 diagram

-- |
-- Default options for rendering a diagram into SVG.
--
-- This is a record value, so specific options can be overridden using record syntax.
-- For example, the width of the rendered image can be overridden:
--
-- >>> let options = defaultOptions & sizeSpec .~ mkWidth 128
-- >>> view sizeSpec options
-- SizeSpec (V2 128.0 0.0)
defaultOptions :: (SVGFloat n) => Options SVG V2 n
defaultOptions = SVGOptions (mkWidth 4096) Nothing "" [] True
