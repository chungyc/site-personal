-- |
-- Description: Rules for generating pages with recent updates.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
--
-- Exports rules generating updates about me or the web site.
module Web.Site.Rules.Update (rules, items, withLatest) where

import Hakyll
import Web.Site.Routes

-- |
-- Rules related to recent updates about me or the web site.
rules :: Rules ()
rules = do
  -- The overview page.
  match "update/index.html" $ do
    route $ constRoute "updates"
    compile $ do
      updates <- recentFirst =<< loadAllSnapshots updatePattern "updates"
      let updatesContext =
            listField "updates" defaultContext (return updates)
              <> defaultContext

      getResourceBody
        >>= applyAsTemplate updatesContext
        >>= loadAndApplyTemplate "templates/default.html" updatesContext

  -- Individual update page.
  match updatePattern $ do
    route stripExtension
    compile $
      pandocCompiler
        >>= saveSnapshot "updates"
        >>= loadAndApplyTemplate "templates/update.html" defaultContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext

  -- RSS feed for updates.
  create ["updates.xml"] $ do
    route idRoute
    compile $ do
      let feedContext = bodyField "description" <> defaultContext
      posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots updatePattern "updates"
      renderRss updateFeedConfiguration feedContext posts

-- |
-- Pattern for files which are individual updates.
--
-- Does not include the overall index for updates.
updatePattern :: Pattern
updatePattern = "update/**" .&&. complement "update/index.html"

-- |
-- Pattern for files matched or created in this module.
--
-- These will be used to generate the sitemap.
items :: Pattern
items = "update/**"

-- |
-- Apply a context with the latest update in the list field @latest-update@ to the given rule.
--
-- In particular, this is used by the front page to include the latest update.
withLatest :: (Context String -> Compiler (Item String)) -> Compiler (Item String)
withLatest f = do
  updates <- fmap (take 1) . recentFirst =<< loadAllSnapshots updatePattern "updates"
  let indexContext =
        if null updates
          then defaultContext
          else listField "latest-update" defaultContext (return updates) <> defaultContext
  f indexContext

-- | Feed configuration for updates.
updateFeedConfiguration :: FeedConfiguration
updateFeedConfiguration =
  FeedConfiguration
    { feedTitle = "Updates for Yoo Chung",
      feedDescription = "Occasional personal updates from Yoo Chung.",
      feedAuthorName = "Yoo Chung",
      feedAuthorEmail = "web@chungyc.org",
      feedRoot = "https://chungyc.org"
    }
