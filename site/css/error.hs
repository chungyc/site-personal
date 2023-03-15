-- |
-- Description: Extra stylesheet generation for error pages.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Main (main) where

import Clay
import Prelude hiding (div)

main :: IO ()
main = putCss errorStyle

-- |
-- Style for error pages which Clay will render into a stylesheet.
errorStyle :: Css
errorStyle = do
  -- For single letters used like a big image all by itself.
  -- E.g., a big question mark for a 404 page.
  div # ".letter-image" ? fontSize (cm 10)
