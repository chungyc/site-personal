-- |
-- Description: Stylesheet generation for the web site.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Web.Site.Styles.Default (style) where

import Clay hiding (style)
import Clay.Media qualified as Media
import Prelude hiding (div, not, rem, (**))

-- |
-- Default style for Clay to render into a stylesheet.
style :: Css
style = do
  html ? do
    fontFamily ["Georgia", "Garamond"] [serif, sansSerif, monospace]
    textRendering optimizeLegibility
    textAlign justify
    lineHeight $ unitless 1.25

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
    a ? paddingRight (rem 1)

  article |> section # ".byline" ? do
    fontFamily ["Verdana"] [sansSerif, serif, monospace]
    fontSize $ rem 0.7
    p ? do
      marginTop $ em 0.2
      marginBottom $ em 0.2

  nav # ".toc" ? do
    marginTop $ em 1
    marginBottom $ em 1
    sym padding $ em 1
    borderStyle solid
    borderWidth $ px 1

    h2 ? do
      fontFamily ["Georgia", "Garamond"] [serif, sansSerif, monospace]
      fontSize $ rem 1.2
      fontStyle normal
      fontWeight bold
      textDecorationLine none
      textDecorationStyle none
      marginTop $ em 0.1
      marginBottom $ em 0.25

    ul <> (ul ** (ul <> li)) ? do
      paddingLeft $ em 0.75
      marginTop $ em 0.1
      marginBottom $ em 0.1
      listStyleType none

  li |+ li ? marginTop (rem 0.75)

  dt ? do
    fontWeight bold
    marginBottom $ em 0.25

  dd ? do
    marginBottom $ em 1

  div # ".sourceCode" ? do
    borderStyle solid
    borderWidth $ px 1
    marginRight $ em 1
    marginLeft $ em 1
    sym padding $ em 0.5

  query Media.all [Media.maxWidth $ cm 5] $ do
    body ? sym margin nil

  query Media.all [Media.minWidth $ cm 21] $ do
    body ? do
      width $ cm 20
      marginRight auto
      marginLeft auto

  query Media.all [Media.prefersColorScheme Media.light] lightColorScheme

  query Media.all [Media.prefersColorScheme Media.dark] darkColorScheme

-- | Styles for headings.
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

  a # link ? color blue
  a # visited ? color purple

  h1 ? fontColor (headingColor 1)
  h2 ? fontColor (headingColor 2)
  h3 ? fontColor (headingColor 3)
  h4 ? fontColor (headingColor 4)
  h5 ? fontColor (headingColor 5)
  h6 ? fontColor (headingColor 6)

  nav # ".toc" ? do
    borderColor lightgrey
    backgroundColor $ rgb 240 240 240

  div # ".sourceCode" ? borderColor lightgrey
  where
    headingColor n = rgb (n * 20) (n * 20) (100 + n * 10)

-- | Color scheme to use in dark mode.
darkColorScheme :: Css
darkColorScheme = do
  html ? do
    color white
    backgroundColor black

  footer ? borderTop (px 1) solid white

  a # link ? color cyan
  a # visited ? color pink

  h1 ? fontColor (headingColor 1)
  h2 ? fontColor (headingColor 2)
  h3 ? fontColor (headingColor 3)
  h4 ? fontColor (headingColor 4)
  h5 ? fontColor (headingColor 5)
  h6 ? fontColor (headingColor 6)

  nav # ".toc" ? do
    borderColor dimgrey
    backgroundColor $ rgb 20 20 20

  div # ".sourceCode" ? borderColor dimgrey
  where
    headingColor n = rgb (255 - n * 20) (255 - n * 20) (155 - n * 10)
