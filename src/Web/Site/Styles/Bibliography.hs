-- |
-- Description: Stylesheet generation for CSL-based bibliography pages.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Web.Site.Styles.Bibliography (style) where

import Clay hiding (style)
import Prelude hiding (div, not, rem)

-- |
-- Bibliography style for Clay to render into a stylesheet.
style :: Css
style = do
  div # ".csl-bib-body" ? do
    paddingTop $ rem 1
    paddingBottom $ rem 1

    div # ".csl-entry" ? do
      marginTop $ rem 1
      marginBottom $ rem 1

      -- When width is small, break long URLs.
      overflowWrap breakWord

      div # ".csl-left-margin" ? do
        float floatLeft
        width $ rem 3
        textAlign $ alignSide sideRight

      div # ".csl-right-inline" ? do
        marginLeft $ rem 3
        paddingLeft $ rem 1
