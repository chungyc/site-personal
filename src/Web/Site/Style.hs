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
    fontSize (rem 0.75)
    borderTop (px 1) solid black
    marginTop (rem 1)

  footer |> nav ? do
    paddingTop (rem 1)
    a ? do
      paddingRight (rem 2)

headings :: Css
headings = do
  h1 ? do
    fontFamily ["Courier New"] [monospace, sansSerif]
    fontSize $ rem 1.5
    fontStyle italic

  h2 ? do
    fontFamily ["Courier New"] [monospace, sansSerif]
    fontSize $ rem 1.3
    textDecorationLine underline
    textDecorationColor midnightblue

  h3 ? do
    fontFamily ["Courier New"] [monospace, sansSerif]
    fontSize $ rem 1.2
    textDecorationLine underline
    textDecorationColor darkblue

  h4 ? do
    fontFamily ["Courier New"] [monospace, sansSerif]
    fontSize $ rem 1.1
    textDecorationLine underline
    textDecorationColor blue

  h5 ? do
    fontFamily ["Courier New"] [monospace, sansSerif]
    fontSize $ rem 1.05
    textDecorationLine underline
    textDecorationColor deepskyblue

  h6 ? do
    fontFamily ["Courier New"] [monospace, sansSerif]
    textDecorationLine underline
    textDecorationColor lightblue
