-- |
-- Description: Rules for generating the site map.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
--
-- Exports rules related to generating the sitemap for the web site.
module Web.Site.Rules.Sitemap (rules) where

import Hakyll
import Web.Site.Compilers

-- |
-- Given a pattern which matches all the pages on the web site,
-- returns the rules which generates the sitemap.
rules :: Pattern -> Rules ()
rules items = do
  match "sitemap.xml" $ do
    route idRoute
    compile $ do
      itemList <- loadAll items
      let sitemapContext =
            listField "items" siteContext (return itemList)
              <> siteContext
      getResourceBody >>= applyAsTemplate sitemapContext
