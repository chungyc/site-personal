-- |
-- Description: Rules for generating stylesheets from Haskell code.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Web.Site.Rules.Stylesheet (rules) where

import Hakyll
import Web.Site.Compilers

-- | Rules related to stylesheets generated from Haskell code.
rules :: Rules ()
rules = do
  match "css/**.hs" $ do
    route $ setExtension "css"
    compile $ getResourceBody >>= haskellCompiler
