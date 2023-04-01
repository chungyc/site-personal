-- |
-- Description: Generation of the default stylesheet for the web site.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Main (main) where

import Clay
import Clay.Media qualified as Media
import Prelude hiding (div, filter, not, (**))

main :: IO ()
main = putCss defaultStyle

-- |
-- Default style for Clay to render into a stylesheet.
defaultStyle :: Css
defaultStyle = do
  -- Comes first, since later styles should be able to override properties.
  genericStyle

  codeStyle
  articleStyle
  tableOfContents
  figures

  -- Comes last, since they may have to override previous properties.
  mediaStyles

-- | Styles which apply generally.
genericStyle :: Css
genericStyle = do
  html ? do
    fontFamily ["Georgia", "Garamond"] [serif, sansSerif, monospace]
    textRendering optimizeLegibility
    textAlign justify
    lineHeight $ unitless 1.25
    "hyphens" -: "auto"

  body ? do
    marginTop $ em 2
    marginLeft $ pct 10
    marginRight $ pct 10
    marginBottom $ em 2

  headings

  footer ? do
    fontFamily ["Courier New"] [monospace, sansSerif]
    fontSize $ em 0.75
    borderTop (px 1) solid black
    marginTop $ em 1

  footer |> nav ? do
    paddingTop $ em 1.5
    a ? paddingRight (em 1)

  li |+ li ? marginTop (em 0.75)

  li |> (ul <> ol) ? marginTop (em 0.75)

  dt ? do
    fontWeight bold
    marginBottom $ em 0.25

  dd ? do
    marginBottom $ em 1

-- | Styles for headings.
headings :: Css
headings = do
  h1 ? do
    fontFamily ["Courier New"] [monospace, sansSerif]
    fontSize $ em 2
    fontStyle italic

  h2 ? do
    common
    fontSize $ em 1.8

  h3 ? do
    common
    fontSize $ em 1.5

  h4 ? do
    common
    fontSize $ em 1.25

  h5 ? do
    common
    fontSize $ em 1.1

  h6 ? common
  where
    common = do
      fontFamily ["Courier New"] [monospace, sansSerif]
      textDecorationLine underline
      textDecorationStyle dotted

-- | Style for code snippets.
codeStyle :: Css
codeStyle = do
  div # ".sourceCode" ? do
    borderStyle solid
    borderWidth $ px 1
    marginRight $ em 1
    marginLeft $ em 1
    sym padding $ em 0.5

-- | Style for @article@ elements.
articleStyle :: Css
articleStyle = do
  article |> section # ".byline" ? do
    fontFamily ["Verdana"] [sansSerif, serif, monospace]
    fontSize $ em 0.7
    p ? do
      marginTop $ em 0.2
      marginBottom $ em 0.2

-- |
-- Style for table of contents.
tableOfContents :: Css
tableOfContents = do
  nav # ".toc" ? do
    marginTop $ em 1
    marginBottom $ em 1
    sym padding $ em 1
    borderStyle solid
    borderWidth $ px 1

    h2 ? do
      fontFamily ["Georgia", "Garamond"] [serif, sansSerif, monospace]
      fontSize $ em 1.2
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

-- |
-- Style for figures generated from Diagrams-based Haskell code.
figures :: Css
figures = do
  figure ? do
    display block
    paddingTop $ em 0.5
    paddingBottom $ em 0.5
    marginTop $ em 1
    marginBottom $ em 1
    marginLeft auto
    marginRight auto
    textAlign center

    img ? do
      display block
      marginLeft auto
      marginRight auto
      maxWidth $ pct 95

    figcaption ? do
      display block
      fontFamily ["Verdana"] [sansSerif, serif, monospace]
      fontSize $ em 0.9
      marginTop $ em 0.5
      marginLeft auto
      marginRight auto

-- | Customize style depending on media.
mediaStyles :: Css
mediaStyles = do
  query Media.all [Media.maxWidth $ em 30] $ do
    body ? sym margin (em 1)

    ul <> ol ? do
      marginLeft $ em 0.5
      paddingLeft $ em 0.5

  query Media.all [Media.minWidth $ em 60] $ do
    body ? do
      width $ em 60
      marginRight auto
      marginLeft auto

  query Media.all [Media.prefersColorScheme Media.light] lightColorScheme

  query Media.all [Media.prefersColorScheme Media.dark] darkColorScheme

-- | Color scheme to use in light mode.
lightColorScheme :: Css
lightColorScheme = do
  html ? do
    color black
    backgroundColor white

  a # link ? color blue
  a # visited ? color purple

  headingColors headingColor

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

  headingColors headingColor

  nav # ".toc" ? do
    borderColor dimgrey
    backgroundColor $ rgb 20 20 20

  figure |> img # (not ".keep-colors" <> "src" $= ".svg") ? do
    filter (invert $ pct 100)

  div # ".sourceCode" ? borderColor dimgrey
  where
    headingColor n = rgb (255 - n * 20) (255 - n * 20) (155 - n * 10)

-- | Set up colors for headings depending on heading level.
headingColors ::
  -- | Maps heading level to its color.
  (Integer -> Color) ->
  -- | Style defining heading colors.
  Css
headingColors mapColor = do
  h1 ? fontColor (mapColor 1)
  h2 ? fontColor (mapColor 2)
  h3 ? fontColor (mapColor 3)
  h4 ? fontColor (mapColor 4)
  h5 ? fontColor (mapColor 5)
  h6 ? fontColor (mapColor 6)
