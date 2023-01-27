-- |
-- Description: Rules for generating the web site.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: yoo.chul.chung@gmail.com
module Web.Site.Rules (rules) where

import Hakyll

-- |
-- Rules for Hakyll to generate the web site.
rules :: Rules ()
rules = do
  match "htaccess" $ do
    route $ constRoute ".htaccess"
    compile copyFileCompiler

  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "errors/missing.html" $ do
    route idRoute
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  match (fromList ["about.markdown", "contact.markdown"]) $ do
    route stripExtension
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

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
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (return posts) <> defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    <> defaultContext

stripExtension :: Routes
stripExtension = setExtension ""
