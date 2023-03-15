-- |
-- Description: Stylesheet generation for light mode syntax highlighting.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Main (main) where

import Text.Pandoc.Highlighting (pygments, styleToCss)

main :: IO ()
main = putStrLn $ styleToCss pygments
