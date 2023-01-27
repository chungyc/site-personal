-- |
-- Description: Stylesheet generation for the web site.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: yoo.chul.chung@gmail.com
module Web.Site.Style (defaultStyle) where

import Clay
import Prelude hiding (rem)

-- |
-- Default style for Clay to render into a stylesheet.
defaultStyle :: Css
defaultStyle = do
  html ? fontFamily ["Georgia", "Garamond"] [serif, sansSerif, monospace]

  body ? do
    marginLeft (pct 10)
    marginRight (pct 10)

  h1 ? do
    fontSize (rem 1.5)
    textAlign (alignSide sideCenter)

  h2 ? do
    fontSize (rem 1.3)
    textAlign (alignSide sideLeft)
