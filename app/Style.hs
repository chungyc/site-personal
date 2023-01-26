-- |
-- Description: Program for generating the style sheets for the web site.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: yoo.chul.chung@gmail.com
module Main (main) where

import Clay
import Data.Text.Lazy.IO (writeFile)
import Web.Site.Style (defaultStyle)
import Prelude hiding (writeFile)

main :: IO ()
main = writeStylesheet "site/css/default.css" defaultStyle

-- |
-- Write the stylesheet rendered from the given style to the given file path.
writeStylesheet :: FilePath -> Css -> IO ()
writeStylesheet path css = writeFile path $ render css
