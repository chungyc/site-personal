-- |
-- Description: Stylesheet generation for CSL-based bibliography pages.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Main (main) where

import Clay
import Prelude hiding (div, not)

main :: IO ()
main = putCss biblioStyle

-- |
-- Bibliography style for Clay to render into a stylesheet.
biblioStyle :: Css
biblioStyle = do
  div # ".csl-bib-body" ? do
    paddingBottom $ em 1

    div # ".csl-entry" ? do
      marginBottom $ em 1

      -- When width is small, break long URLs.
      overflowWrap breakWord

      div # ".csl-left-margin" ? do
        float floatLeft
        width $ em 3
        textAlign $ alignSide sideRight

      div # ".csl-right-inline" ? do
        marginLeft $ em 3
        paddingLeft $ em 1
