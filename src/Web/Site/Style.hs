-- |
-- Description: Stylesheet generation for the web site.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: yoo.chul.chung@gmail.com
module Web.Site.Style (defaultStyle) where

import Clay
import Clay.Media qualified as Media
import Prelude hiding (rem, (**))

-- |
-- Default style for Clay to render into a stylesheet.
defaultStyle :: Css
defaultStyle = do
  html ? fontSize (pct 62.5)

  body ? do
    fontSize (rem 1.6)
    color (rgb 0 0 0)

  header ? borderBottom (rem 0.2) solid (rgb 0 0 0)

  nav ? textAlign (alignSide sideRight)

  nav ** a ? do
    fontSize (rem 1.8)
    fontWeight bold
    color black
    textDecoration none
    textTransform uppercase

  footer ? do
    marginTop (rem 3)
    sym2 padding (rem 1.2) nil
    borderTop (rem 0.2) solid (rgb 0 0 0)
    fontSize (rem 1.2)
    color (rgb (5 * 16) (5 * 16) (5 * 16))

  h1 ? fontSize (rem 2.4)

  h2 ? fontSize (rem 2)

  article ** ".header" ? do
    fontSize (rem 1.4)
    fontStyle italic
    color (rgb (5 * 16) (5 * 16) (5 * 16))

  ".logo" ** a ? do
    fontWeight bold
    color (rgb 0 0 0)
    textDecoration none

  query Media.screen [Media.maxWidth (px 319)] $ do
    body ? do
      width (pct 90)
      sym margin nil
      sym2 padding nil (pct 5)

    header ? sym2 margin (rem 4.2) nil

    nav ? do
      sym3 margin nil auto (rem 3)
      textAlign center

    footer ? textAlign center

    ".logo" ? do
      textAlign center
      sym3 margin (rem 1) auto (rem 3)

    ".logo" ** a ? fontSize (rem 2.4)

    nav ** a ? do
      display block
      lineHeight (unitless 1.6)

  query Media.screen [Media.minWidth (px 320)] $ do
    body ? do
      width (pct 90)
      sym margin nil
      sym2 padding nil (pct 5)

    header ? sym2 margin (rem 4.2) nil

    nav ? do
      sym3 margin nil auto (rem 3)
      textAlign center

    footer ? textAlign center

    ".logo" ? do
      textAlign center
      sym3 margin (rem 1) auto (rem 3)

    ".logo" ** a ? fontSize (rem 2.4)

    nav ** a ? do
      display inline
      sym2 margin nil (rem 0.6)

  query Media.screen [Media.minWidth (px 640)] $ do
    body ? do
      width (rem 60)
      sym2 margin nil auto
      sym padding nil

    header ? do
      sym3 margin nil nil (rem 3)
      sym2 padding (rem 1.2) nil

    nav ? do
      sym margin nil
      textAlign (alignSide sideRight)

    nav ** a ? do
      margin nil nil nil (rem 1.2)
      display inline

    footer ? textAlign (alignSide sideRight)

    ".logo" ? do
      sym margin nil
      textAlign (alignSide sideLeft)

    ".logo" ** a ? do
      float floatLeft
      fontSize (rem 1.8)
