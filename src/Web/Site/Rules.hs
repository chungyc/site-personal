-- |
-- Description: Rules for generating the web site.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: yoo.chul.chung@gmail.com
module Web.Site.Rules (rules) where

import Hakyll
import Web.Site.Routes
import Web.Site.Rules.File qualified as File
import Web.Site.Rules.Update qualified as Update

-- |
-- Rules for Hakyll to generate the web site.
rules :: Rules ()
rules = do
  File.rules
  Update.rules

  match "templates/*" $ compile templateBodyCompiler

  match "htaccess" $ do
    route $ constRoute ".htaccess"
    compile copyFileCompiler

  match "errors/missing.html" $ do
    route idRoute
    compile $
      getResourceBody
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  match (fromList ["about.markdown", "contact.markdown"]) $ do
    route stripExtension
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $
      getResourceBody
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls
