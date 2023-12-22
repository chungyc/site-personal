-- |
-- Description: Rules for generating pages with recent updates.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
--
-- Exports rules generating updates about me or the web site.
module Web.Site.Rules.Update (rules, items, withLatest) where

import Hakyll
import Web.Site.Compilers
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
            listField "updates" siteContext (return updates)
              <> siteContext

      getResourceBody
        >>= applyAsTemplate updatesContext
        >>= loadAndApplyTemplate "templates/default.html" updatesContext

  -- Individual update page.
  match updatePattern $ do
    route dropExtensions
    compile $
      pandocCompiler
        >>= saveSnapshot "updates"
        >>= loadAndApplyTemplate "templates/update.html" siteContext
        >>= loadAndApplyTemplate "templates/default.html" siteContext

  -- RSS feed for updates.
  create ["updates.xml"] $ do
    route idRoute
    compile $ do
      let feedContext = bodyField "description" <> siteContext
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
items = "update/**" .||. "updates.xml"

-- |
-- Apply a context with the latest update in the list field @latest-update@ to the given rule.
--
-- In particular, this is used by the front page to include the latest update.
-- The latest update with have a @latest-update@ metadata field with a true value
-- and a @teaser@ metadata field with the teaser if it exists.
withLatest :: (Context String -> Compiler (Item String)) -> Compiler (Item String)
withLatest f = do
  updates <- fmap (take 1) . recentFirst =<< loadAllSnapshots updatePattern "updates"
  let indexContext
        | [] <- updates = siteContext
        | otherwise = listField "latest-update" updateContext (pure updates) <> siteContext
  f indexContext
  where
    updateContext =
      mconcat
        [ boolField "latest-update" (const True),
          teaserField "teaser" "updates",
          siteContext
        ]

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
