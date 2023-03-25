-- |
-- Description: Extra stylesheet generation for front page.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Main (main) where

import Clay
import Clay.Media qualified as Media

main :: IO ()
main = putCss frontStyle

-- |
-- Style for front page which Clay will render into a stylesheet.
frontStyle :: Css
frontStyle = do
  ".latest-update" ? do
    sym padding $ em 1

  query Media.all [Media.minWidth $ cm 10] $ do
    ".latest-update" ? do
      sym margin $ em 2
      sym padding $ em 2

  query Media.all [Media.prefersColorScheme Media.light] $ do
    ".latest-update" ? do
      border (px 2) solid $ rgb 20 20 50
      backgroundColor $ rgb 250 250 250

  query Media.all [Media.prefersColorScheme Media.dark] $ do
    ".latest-update" ? do
      border (px 2) solid $ rgb 230 230 200
      backgroundColor $ rgb 5 5 5
