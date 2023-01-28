-- |
-- Description: Rules for generating the web site.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: yoo.chul.chung@gmail.com
module Web.Site.Rules (rules) where

import Hakyll
import Web.Site.Routes
import Web.Site.Rules.File qualified as File
import Web.Site.Rules.Server qualified as Server
import Web.Site.Rules.Update qualified as Update

-- |
-- Rules for Hakyll to generate the web site.
rules :: Rules ()
rules = do
  Server.rules
  File.rules
  Update.rules

  match "templates/*" $ compile templateBodyCompiler

  match (fromList ["about.markdown", "contact.markdown"]) $ do
    route stripExtension
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      updates <- fmap (take 1) . recentFirst =<< loadAllSnapshots "update/**" "content"
      let indexContext =
            if null updates
              then defaultContext
              else listField "latest-update" defaultContext (return updates) <> defaultContext
      getResourceBody
        >>= applyAsTemplate indexContext
        >>= loadAndApplyTemplate "templates/default.html" indexContext
        >>= relativizeUrls
