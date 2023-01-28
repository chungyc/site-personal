-- |
-- Description: Rules for generating pages with recent updates.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: yoo.chul.chung@gmail.com
module Web.Site.Rules.Update (rules) where

import Hakyll
import Web.Site.Routes

-- |
-- Rules related to generated recent updates from me.
rules :: Rules ()
rules = do
  create ["updates"] $ do
    route idRoute
    compile $ do
      updates <- recentFirst =<< loadAll "update/**"
      let updatesContext =
            listField "updates" defaultContext (return updates)
              <> constField "title" "Updates"
              <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/updates.html" updatesContext
        >>= loadAndApplyTemplate "templates/default.html" updatesContext
        >>= relativizeUrls

  match "update/**" $ do
    route stripExtension
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/update.html" defaultContext
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls
