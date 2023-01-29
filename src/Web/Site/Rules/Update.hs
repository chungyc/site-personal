-- |
-- Description: Rules for generating pages with recent updates.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: yoo.chul.chung@gmail.com
module Web.Site.Rules.Update (rules, withLatest) where

import Hakyll
import Web.Site.Routes

-- |
-- Rules related to generated recent updates from me.
rules :: Rules ()
rules = do
  -- The overview page.
  create ["updates"] $ do
    route idRoute
    compile $ do
      updates <- recentFirst =<< loadAll "update/**"
      let updatesContext =
            listField "updates" defaultContext (return updates)
              <> constField "title" "Updates"
              <> constField "rss-feed-link" "/updates.rss"
              <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/updates.html" updatesContext
        >>= loadAndApplyTemplate "templates/default.html" updatesContext
        >>= relativizeUrls

  -- Individual update page.
  match "update/**" $ do
    route stripExtension
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/update.html" defaultContext
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  -- RSS feed for updates.
  create ["updates.rss"] $ do
    route idRoute
    compile $ do
      let feedContext = bodyField "description" <> defaultContext
      posts <- recentFirst =<< loadAllSnapshots "update/**" "content"
      renderRss updateFeedConfiguration feedContext posts

-- | Apply a context with the latest update in the list field @latest-update@ to the given rule.
withLatest :: (Context String -> Compiler (Item String)) -> Compiler (Item String)
withLatest f = do
  updates <- fmap (take 1) . recentFirst =<< loadAllSnapshots "update/**" "content"
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
      feedRoot = "https://chungyc.org/updates"
    }
