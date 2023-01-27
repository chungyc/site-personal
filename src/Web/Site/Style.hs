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
  html ? do
    fontFamily ["Georgia", "Garamond"] [serif, sansSerif, monospace]
    textRendering optimizeLegibility

  body ? do
    marginTop $ rem 2
    marginLeft $ pct 10
    marginRight $ pct 10
    marginBottom $ rem 2

  headings

  footer ? do
    fontFamily ["Courier New"] [monospace, sansSerif]
    fontSize $ rem 0.75
    borderTop (px 1) solid black
    marginTop $ rem 1

  footer |> nav ? do
    paddingTop $ rem 1
    a ? paddingRight (rem 2)

headings :: Css
headings = do
  h1 ? do
    fontFamily ["Courier New"] [monospace, sansSerif]
    fontSize $ rem 1.5
    fontStyle italic

  h2 ? do
    common
    fontSize $ rem 1.3
    textDecorationColor midnightblue

  h3 ? do
    common
    fontSize $ rem 1.2
    textDecorationColor darkblue

  h4 ? do
    common
    fontSize $ rem 1.1
    textDecorationColor blue

  h5 ? do
    common
    fontSize $ rem 1.05
    textDecorationColor deepskyblue

  h6 ? do
    common
    textDecorationColor lightblue
  where
    common = do
      fontFamily ["Courier New"] [monospace, sansSerif]
      textDecorationLine underline
      textDecorationStyle dotted
