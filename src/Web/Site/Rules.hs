-- |
-- Description: Rules for generating the web site.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Web.Site.Rules (rules) where

import Hakyll
import Web.Site.Routes
import Web.Site.Rules.Article qualified as Article
import Web.Site.Rules.File qualified as File
import Web.Site.Rules.Link qualified as Link
import Web.Site.Rules.Publication qualified as Publication
import Web.Site.Rules.Server qualified as Server
import Web.Site.Rules.Sitemap qualified as Sitemap
import Web.Site.Rules.Update qualified as Update

-- |
-- Rules for Hakyll to generate the web site.
rules :: Rules ()
rules = do
  Server.rules
  File.rules
  Update.rules
  Article.rules
  Publication.rules
  Link.rules

  match "templates/*" $ compile templateBodyCompiler

  match (fromList ["about.markdown"]) $ do
    route stripExtension
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext

  match "index.html" $ do
    route idRoute
    compile $ Update.withLatest $ \indexContext ->
      getResourceBody
        >>= applyAsTemplate indexContext
        >>= loadAndApplyTemplate "templates/default.html" indexContext

  Sitemap.rules $
    "about.markdown"
      .||. "index.html"
      .||. Article.items
      .||. Link.items
      .||. Publication.items
      .||. Update.items
