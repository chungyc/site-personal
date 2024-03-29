-- |
-- Description: Rules for files included verbatim.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
--
-- Exports the rules for files that are to be included as is on the site.
module Web.Site.Rules.File (rules) where

import Hakyll

-- | Rules related to files that are copied as is.
rules :: Rules ()
rules = do
  match "images/**" $ do
    route idRoute
    compile copyFileCompiler

  match "files/**" $ do
    route idRoute
    compile copyFileCompiler
