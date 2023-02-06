-- |
-- Description: Rules for generating pages with generic articles.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Web.Site.Rules.Article (rules) where

import Hakyll
import Web.Site.Routes

-- |
-- Rules related to generic articles.
--
-- I.e., articles that are not specifically updates about things related to me.
rules :: Rules ()
rules = do
  -- Individual articles.
  match "article/**" $ do
    route stripExtension
    compile $
      pandocCompiler
        >>= saveSnapshot "articles"
        >>= loadAndApplyTemplate "templates/article.html" defaultContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext

  -- The overview page.
  match "articles.html" $ do
    route stripExtension
    compile $ do
      articles <- recentFirst =<< loadAllSnapshots "article/**" "articles"
      let context =
            listField "articles" defaultContext (return articles)
              <> defaultContext

      getResourceBody
        >>= applyAsTemplate context
        >>= loadAndApplyTemplate "templates/default.html" context

  -- RSS feed for articles.
  create ["articles.xml"] $ do
    route idRoute
    compile $ do
      let feedContext = bodyField "description" <> defaultContext
      articles <- recentFirst =<< loadAllSnapshots "article/**" "articles"
      renderRss updateFeedConfiguration feedContext articles

-- |
-- Feed configuration for updates.
updateFeedConfiguration :: FeedConfiguration
updateFeedConfiguration =
  FeedConfiguration
    { feedTitle = "Articles by Yoo Chung",
      feedDescription = "Articles written by Yoo Chung and posted on their personal web site.",
      feedAuthorName = "Yoo Chung",
      feedAuthorEmail = "web@chungyc.org",
      feedRoot = "https://chungyc.org"
    }