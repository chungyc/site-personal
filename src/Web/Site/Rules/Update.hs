-- |
-- Description: Rules for generating pages with recent updates.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Web.Site.Rules.Update (rules, items, withLatest) where

import Hakyll
import Web.Site.Routes

-- |
-- Rules related to generated recent updates from me.
rules :: Rules ()
rules = do
  -- The overview page.
  match "updates.html" $ do
    route stripExtension
    compile $ do
      updates <- recentFirst =<< loadAllSnapshots "update/**" "updates"
      let updatesContext =
            listField "updates" defaultContext (return updates)
              <> constField "title" "Updates"
              <> constField "rss-feed-link" "/updates.rss"
              <> defaultContext

      getResourceBody
        >>= applyAsTemplate updatesContext
        >>= loadAndApplyTemplate "templates/default.html" updatesContext

  -- Individual update page.
  match "update/**" $ do
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
      posts <- recentFirst =<< loadAllSnapshots "update/**" "updates"
      renderRss updateFeedConfiguration feedContext posts

-- |
-- Pattern for files matched or created in this module.
items :: Pattern
items = "updates.html" .||. "update/**"

-- | Apply a context with the latest update in the list field @latest-update@ to the given rule.
withLatest :: (Context String -> Compiler (Item String)) -> Compiler (Item String)
withLatest f = do
  updates <- fmap (take 1) . recentFirst =<< loadAllSnapshots "update/**" "updates"
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
