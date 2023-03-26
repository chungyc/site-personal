-- |
-- Description: Extra stylesheet generation for pages in the links collection.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Main (main) where

import Clay
import Prelude hiding (div)

main :: IO ()
main = putCss linksStyle

-- |
-- Style for link collection pages which Clay will render into a stylesheet.
--
-- Mainly for reducing the heading sizes.
linksStyle :: Css
linksStyle = do
  h2 ? fontSize (em 1.4)
  h3 ? fontSize (em 1.3)
  h4 ? fontSize (em 1.2)
  h5 ? fontSize (em 1.1)
  h6 ? fontSize (em 1)
