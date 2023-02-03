-- |
-- Description: Rules for files included verbatim.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Web.Site.Rules.Link (rules) where

import Hakyll
import System.FilePath (dropExtension, takeDirectory)

rules :: Rules ()
rules = do
  match "links/**" $ do
    route $ customRoute toIndexFilePath -- gsubRoute ".markdown" (const "/index.html")
    compile linksCompiler

  match "links.markdown" $ do
    route $ constRoute "links/index.html"
    compile linksCompiler

-- |
-- From the given identifier, return the path to the index HTML file
-- as if the original file was actually a directory.
--
-- I.e., all of the files in @links/@ will be turned into @index.html@ files
-- in a directory so that their URLs will end with the directory.
-- E.g., @links/fun.markdown@ would have the URL ending with @links/fun/@.
-- This allows them to have sub-categories without using separate names
-- or having to include the HTML file name extension in the URL.
toIndexFilePath :: Identifier -> FilePath
toIndexFilePath identifier = dropExtension path ++ "/index.html"
  where
    path = toFilePath identifier

-- |
-- Compiles a page with links to also link to its sub-categories and parent.
-- (Note: linking to parent is still pending.)
linksCompiler :: Compiler (Item String)
linksCompiler = do
  path <- toFilePath <$> getUnderlying
  subcategories <-
    loadAllSnapshots
      (fromGlob $ dropExtension path ++ "/*")
      $ "links:" ++ dropExtension path

  let linksContext
        | null subcategories = defaultContext
        | otherwise =
            listField "subcategories" defaultContext (return subcategories)
              <> defaultContext

  pandocCompiler
    >>= saveSnapshot ("links:" ++ takeDirectory path)
    >>= loadAndApplyTemplate "templates/links.html" linksContext
    >>= loadAndApplyTemplate "templates/default.html" linksContext
    >>= cleanupIndexUrls

-- |
-- For local URLs which end with @index.html@, strip it.
cleanupIndexUrls :: Item String -> Compiler (Item String)
cleanupIndexUrls = return . fmap (withUrls cleanupIndexUrl)

-- |
-- If the given URL is local and ends with @index.html@, strip the latter.
cleanupIndexUrl :: String -> String
cleanupIndexUrl url@('/' : _)
  | Nothing <- prefix = url
  | Just s <- prefix = s
  where
    prefix = needlePrefix "index.html" url
cleanupIndexUrl url = url
