-- |
-- Description: Rules for generating the publications page.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
--
-- Exports rules for my list of publications.
module Web.Site.Rules.Publication (rules, items) where

import Hakyll

-- |
-- Rules related to the list of my publications.
rules :: Rules ()
rules = do
  match "publications/index.markdown" $ do
    route $ constRoute "publications"
    compile $
      publicationsCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext

  match (fromGlob cslFile) $ compile cslCompiler
  match (fromGlob bibFile) $ compile biblioCompiler

-- |
-- Pattern for files matched or created in this module.
--
-- These will be used to generate the sitemap.
items :: Pattern
items = "publications/index.markdown"

publicationsCompiler :: Compiler (Item String)
publicationsCompiler = do
  csl <- load $ fromFilePath cslFile
  bib <- load $ fromFilePath bibFile
  body <- getResourceBody
  doc <- readPandocBiblio defaultHakyllReaderOptions csl bib body
  return $ writePandoc doc

-- |
-- This is a modified version of the ACM's CSL file.
--
-- The difference is that this sorts by date in reverse order.
cslFile :: String
cslFile = "publications/acm.csl"

-- | This is a bibliography of my publications.
bibFile :: String
bibFile = "publications/chungyc.bib"
