-- |
-- Description: Stylesheet generation for syntax highlighting.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Main (main) where

import Clay
import Prelude hiding (div)

main :: IO ()
main = do
  putStrLn "@import \"syntax-light.css\" all and (prefers-color-scheme: light);"
  putStrLn "@import \"syntax-dark.css\" all and (prefers-color-scheme: dark);"
