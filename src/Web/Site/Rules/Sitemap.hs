-- |
-- Description: Rules for generating the site map.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Web.Site.Rules.Sitemap (rules) where

import Hakyll

-- |
-- Given a pattern which matches all the pages on the web site,
-- returns the rules which generates the site map.
rules :: Pattern -> Rules ()
rules items = do
  match "sitemap.xml" $ do
    route idRoute
    compile $ do
      itemList <- loadAll items

      let itemContext =
            functionField "clean" clean
              <> defaultContext

      let sitemapContext =
            listField "items" itemContext (return itemList)
              <> defaultContext

      getResourceBody
        >>= applyAsTemplate sitemapContext

-- |
-- If the given URL is local and ends with @index.html@, strip the latter.
-- Also turn a local link into a full URL.
clean :: [String] -> Item a -> Compiler String
clean [url@('/' : _)] _
  | Nothing <- prefix = return $ root ++ url
  | Just s <- prefix = return $ root ++ s
  where
    prefix = needlePrefix "index.html" url
    root = "https://chungyc.org"
clean [url] _ = return url
clean _ _ = error "wrong number of arguments"
