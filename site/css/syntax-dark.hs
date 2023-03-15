-- |
-- Description: Stylesheet generation for dark mode syntax highlighting.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Main (main) where

import Text.Pandoc.Highlighting (styleToCss, zenburn)

main :: IO ()
main = putStrLn $ styleToCss zenburn
