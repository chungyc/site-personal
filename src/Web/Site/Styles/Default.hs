-- |
-- Description: Stylesheet generation for the web site.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: yoo.chul.chung@gmail.com
module Web.Site.Styles.Default (style) where

import Clay hiding (style)
import Clay.Media qualified as Media
import Prelude hiding (rem)

-- |
-- Default style for Clay to render into a stylesheet.
style :: Css
style = do
  html ? do
    fontFamily ["Georgia", "Garamond"] [serif, sansSerif, monospace]
    textRendering optimizeLegibility
    textAlign justify
    color black
    backgroundColor white

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

  article |> section # ".byline" ? do
    fontFamily ["Verdana"] [sansSerif, serif, monospace]
    fontSize $ rem 0.7

  query Media.screen [Media.minWidth $ cm 21] $ do
    body ? do
      width $ cm 20
      marginRight auto
      marginLeft auto

headings :: Css
headings = do
  h1 ? do
    fontFamily ["Courier New"] [monospace, sansSerif]
    fontSize $ rem 2
    fontStyle italic
    fontColor $ headingColor 1

  h2 ? do
    common
    level 2
    fontSize $ rem 1.8

  h3 ? do
    common
    level 3
    fontSize $ rem 1.5

  h4 ? do
    common
    level 4
    fontSize $ rem 1.25

  h5 ? do
    common
    level 5
    fontSize $ rem 1.1

  h6 ? do
    common
    level 6
  where
    common = do
      fontFamily ["Courier New"] [monospace, sansSerif]
      fontColor midnightblue
      textDecorationLine underline
      textDecorationStyle dotted

    level n = fontColor $ headingColor n

    headingColor n = rgb (n * 20) (n * 20) (100 + n * 10)
