-- |
-- Description: Rules for generating pages with generic articles.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Web.Site.Rules.Article (rules, items) where

import Hakyll
import Web.Site.Compilers
import Web.Site.Routes

-- |
-- Rules related to generic articles.
--
-- I.e., articles that are not specifically updates about things related to me.
rules :: Rules ()
rules = do
  -- Individual articles.
  match articlePattern $ do
    route $
      composeRoutes stripExtension $
        -- Index pages have should URLs to the directory.
        gsubRoute "/index$" (const "/index.html")

    compile $
      articleCompiler
        >>= saveSnapshot "articles"
        >>= loadAndApplyTemplate "templates/article.html" defaultContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext

  -- The overview page.
  match "article/index.html" $ do
    route $ constRoute "articles"
    compile $ do
      articles <- recentFirst =<< loadAllSnapshots articlePattern "articles"
      let context =
            listField "articles" defaultContext (return articles)
              <> defaultContext

      getResourceBody
        >>= applyAsTemplate context
        >>= loadAndApplyTemplate "templates/default.html" context
        >>= cleanupIndexUrls

  -- RSS feed for articles.
  create ["articles.xml"] $ do
    route idRoute
    compile $ do
      let itemContext = metadataField <> bodyField "description" <> defaultContext
      articles <- fmap (take 10) . recentFirst =<< loadAllSnapshots articlePattern "articles"
      renderRss updateFeedConfiguration itemContext articles
        >>= cleanupIndexUrls

  match "article/bibliography/references.bib" $ compile biblioCompiler
  match "article/bibliography/acm.csl" $ compile cslCompiler

-- |
-- Pattern for files which are individual articles.
--
-- Does not include the overall index for the articles.
articlePattern :: Pattern
articlePattern =
  "article/**"
    .&&. complement "article/index.html"
    .&&. complement "article/bibliography/**"

-- |
-- Pattern for files matched or created in this module.
items :: Pattern
items = articlePattern .||. "article/index.html" .||. "articles.xml"

-- |
-- The Pandoc compiler, but with math support and bibliography.
articleCompiler :: Compiler (Item String)
articleCompiler = do
  body <- getResourceBody
  bibFile <- load "article/bibliography/references.bib"
  cslFile <- load "article/bibliography/acm.csl"
  pandoc <- readPandocWith mathReaderOptions body
  pandoc' <- processPandocBiblio cslFile bibFile pandoc
  return $ writePandocWith mathWriterOptions pandoc'

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
