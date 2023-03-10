-- |
-- Description: Rules for files included verbatim.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Web.Site.Rules.Link (rules, items) where

import Hakyll
import System.FilePath (dropExtension, takeDirectory)
import Web.Site.Compilers

rules :: Rules ()
rules = do
  match "links/**" $ do
    route $ customRoute toIndexFilePath
    compile linksCompiler

  match "links.markdown" $ do
    route $ constRoute "links/index.html"
    compile linksCompiler

-- |
-- Pattern for files matched or created in this module.
items :: Pattern
items = "links/**" .||. "links.markdown"

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
linksCompiler :: Compiler (Item String)
linksCompiler = do
  path <- dropExtension . toFilePath <$> getUnderlying
  subcategories <- loadAllSnapshots (fromGlob $ path ++ "/*") $ "links:" ++ path

  let parent = takeDirectory path

  let applyParent
        | path == "links" = id
        | otherwise = (<>) $ constField "parent-url" $ "/" ++ parent ++ "/"

  let applySubcategories
        | null subcategories = id
        | otherwise = (<>) $ listField "subcategories" defaultContext (return subcategories)

  let linksContext = applyParent . applySubcategories $ defaultContext

  pandocCompiler
    >>= saveSnapshot ("links:" ++ parent)
    >>= loadAndApplyTemplate "templates/links.html" linksContext
    >>= loadAndApplyTemplate "templates/default.html" linksContext
    >>= cleanupIndexUrls
