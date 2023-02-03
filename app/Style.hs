-- |
-- Description: Program for generating the style sheets for the web site.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Main (main) where

import Clay
import Data.Text.Lazy.IO (writeFile)
import Web.Site.Styles.Bibliography qualified as Bibliography
import Web.Site.Styles.Default qualified as Default
import Web.Site.Styles.Error qualified as Error
import Web.Site.Styles.Front qualified as Front
import Web.Site.Styles.Links qualified as Links
import Prelude hiding (writeFile)

main :: IO ()
main = do
  writeStylesheet "site/css/default.css" Default.style
  writeStylesheet "site/css/error.css" Error.style
  writeStylesheet "site/css/front.css" Front.style
  writeStylesheet "site/css/links.css" Links.style
  writeStylesheet "site/css/bibliography.css" Bibliography.style

-- |
-- Write the stylesheet rendered from the given style to the given file path.
writeStylesheet :: FilePath -> Css -> IO ()
writeStylesheet path css = writeFile path $ render css
