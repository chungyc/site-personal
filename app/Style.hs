-- |
-- Description: Program for generating the style sheets for the web site.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: yoo.chul.chung@gmail.com
module Main (main) where

import Clay
import Data.Text.Lazy.IO (writeFile)
import Web.Site.Styles.Default qualified as Default
import Web.Site.Styles.Error qualified as Error
import Web.Site.Styles.Front qualified as Front
import Prelude hiding (writeFile)

main :: IO ()
main = do
  writeStylesheet "site/css/default.css" Default.style
  writeStylesheet "site/css/error.css" Error.style
  writeStylesheet "site/css/front.css" Front.style

-- |
-- Write the stylesheet rendered from the given style to the given file path.
writeStylesheet :: FilePath -> Css -> IO ()
writeStylesheet path css = writeFile path $ render css
