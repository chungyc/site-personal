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

  query Media.all [Media.maxWidth $ cm 5] $ do
    body ? sym margin nil

  query Media.all [Media.minWidth $ cm 21] $ do
    body ? do
      width $ cm 20
      marginRight auto
      marginLeft auto

  query Media.all [Media.prefersColorScheme Media.light] lightColorScheme

  query Media.all [Media.prefersColorScheme Media.dark] darkColorScheme

headings :: Css
headings = do
  h1 ? do
    fontFamily ["Courier New"] [monospace, sansSerif]
    fontSize $ rem 2
    fontStyle italic

  h2 ? do
    common
    fontSize $ rem 1.8

  h3 ? do
    common
    fontSize $ rem 1.5

  h4 ? do
    common
    fontSize $ rem 1.25

  h5 ? do
    common
    fontSize $ rem 1.1

  h6 ? common
  where
    common = do
      fontFamily ["Courier New"] [monospace, sansSerif]
      textDecorationLine underline
      textDecorationStyle dotted

-- | Color scheme to use in light mode.
lightColorScheme :: Css
lightColorScheme = do
  html ? do
    color black
    backgroundColor white

  h1 ? fontColor (headingColor 1)
  h2 ? fontColor (headingColor 2)
  h3 ? fontColor (headingColor 3)
  h4 ? fontColor (headingColor 4)
  h5 ? fontColor (headingColor 5)
  h6 ? fontColor (headingColor 6)
  where
    headingColor n = rgb (n * 20) (n * 20) (100 + n * 10)

-- | Color scheme to use in dark mode.
darkColorScheme :: Css
darkColorScheme = do
  html ? do
    color white
    backgroundColor black

  h1 ? fontColor (headingColor 1)
  h2 ? fontColor (headingColor 2)
  h3 ? fontColor (headingColor 3)
  h4 ? fontColor (headingColor 4)
  h5 ? fontColor (headingColor 5)
  h6 ? fontColor (headingColor 6)
  where
    headingColor n = rgb (255 - n * 20) (255 - n * 20) (155 - n * 10)
