-- |
-- Description: Rules for generating images from Haskell code based on Diagrams.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Web.Site.Rules.Diagram (rules) where

import Hakyll
import Web.Site.Compilers

-- | Rules related to images generated from Haskell code based on Diagrams.
rules :: Rules ()
rules = do
  match "diagrams/**.hs" $ do
    route $ setExtension "svg"
    compile haskellCompiler
