-- |
-- Description: Rules for generating the web site.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: yoo.chul.chung@gmail.com
module Web.Site.Rules (rules) where

import Hakyll
import Web.Site.Routes
import Web.Site.Rules.Update qualified as Update

-- |
-- Rules for Hakyll to generate the web site.
rules :: Rules ()
rules = do
  match "templates/*" $ compile templateBodyCompiler

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

  Update.rules

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    <> defaultContext
