-- |
-- Description: Rules for files included verbatim.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: yoo.chul.chung@gmail.com
module Web.Site.Rules.File (rules) where

import Hakyll

-- | Rules related to files that are copied as is.
rules :: Rules ()
rules = do
  match "images/**" $ do
    route idRoute
    compile copyFileCompiler

  match "css/**" $ do
    route idRoute
    compile compressCssCompiler
