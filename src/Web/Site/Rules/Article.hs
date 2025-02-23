-- |
-- Description: Rules for generating pages with generic articles.
-- Copyright: Copyright (C) 2024 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
--
-- Exports the rules for generic articles on the site.
module Web.Site.Rules.Article (rules, items) where

import Data.ByteString.Lazy.Char8 (unpack)
import Hakyll
import Text.Pandoc.Builder (setMeta)
import Web.Site.Compilers
import Web.Site.Routes

-- |
-- Rules related to generic articles.
--
-- I.e., articles that are not specifically updates about things related to me or this site.
-- In particular, they will usually not have content that is time-sensitive.
--
-- Articles have support for:
--
-- * Rendering math.
-- * Bibliographic references.
-- * Table of contents.
--
-- See the [guide on writing for the site](https://chungyc.org/article/technical/website/guide)
-- for how to enable these for individual pages.
rules :: Rules ()
rules = do
  -- Individual articles.
  match (articlePattern .&&. complement "article/**.hs") $ do
    route $
      composeRoutes dropExtensions $
        -- Index pages have should URLs to the directory.
        gsubRoute "/index$" (const "/index.html")

    compile $
      articleCompiler
        >>= saveSnapshot "articles"
        >>= loadAndApplyTemplate "templates/article.html" siteContext
        >>= loadAndApplyTemplate "templates/default.html" siteContext

  -- Programmatically generated articles.
  -- They should generate content that goes into the <body> element.
  match "article/**.hs" $ do
    route $
      composeRoutes dropExtensions $
        -- Index pages have should URLs to the directory.
        gsubRoute "/index$" (const "/index.html")

    compile $
      haskellCompiler []
        >>= pure . fmap unpack
        >>= saveSnapshot "articles"
        >>= loadAndApplyTemplate "templates/article.html" siteContext
        >>= loadAndApplyTemplate "templates/default.html" siteContext

  -- A curated index to the articles.
  match "article/index.markdown" $ do
    route $ constRoute "articles"
    compile $
      articleCompiler
        >>= loadAndApplyTemplate "templates/default.html" siteContext

  -- The archive page with links to all articles.
  match "article/archive.html" $ do
    route $ constRoute "article/archive"
    compile $ do
      articles <- recentFirst =<< loadAllSnapshots articlePattern "articles"
      let context =
            listField "articles" siteContext (return articles)
              <> siteContext

      getResourceBody
        >>= applyAsTemplate context
        >>= loadAndApplyTemplate "templates/default.html" context

  -- RSS feed for articles.
  create ["articles.xml"] $ do
    route idRoute
    compile $ do
      let itemContext = metadataField <> bodyField "description" <> siteContext
      articles <- fmap (take 10) . recentFirst =<< loadAllSnapshots articlePattern "articles"
      renderRss updateFeedConfiguration itemContext articles

  match "article/bibliography/references.bib" $ compile biblioCompiler
  match "article/bibliography/acm.csl" $ compile cslCompiler

-- |
-- Pattern for files which are individual articles.
--
-- Does not include the overall index for the articles.
articlePattern :: Pattern
articlePattern =
  "article/**"
    .&&. complement "article/archive.html"
    .&&. complement "article/index.markdown"
    .&&. complement "article/bibliography/**"
    .&&. complement "article/**.metadata"

-- |
-- Pattern for files matched or created in this module.
--
-- These will be used to generate the sitemap.
items :: Pattern
items =
  articlePattern
    .||. "article/index.markdown"
    .||. "article/archive.html"

-- |
-- The Pandoc compiler, but with support for:
--
-- * rendering math
-- * bibliographic references
-- * table of contents
articleCompiler :: Compiler (Item String)
articleCompiler = do
  let readerOptions = mathReaderWith defaultHakyllReaderOptions
  writerOptions <- getTocOptionsWith $ mathWriterWith defaultHakyllWriterOptions
  bibFile <- load "article/bibliography/references.bib"
  cslFile <- load "article/bibliography/acm.csl"
  getResourceBody
    >>= readPandocWith readerOptions
    >>= pure . fmap (setMeta "link-citations" True)
    >>= processPandocBiblio cslFile bibFile
    >>= pure . writePandocWith writerOptions

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
