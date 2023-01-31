-- |
-- Description: Rules for generating the publications page.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Web.Site.Rules.Publication (rules) where

import Hakyll

rules :: Rules ()
rules = do
  match "publications/index.markdown" $ do
    route $ constRoute "publications"
    compile $
      publicationsCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext

  match (fromGlob cslFile) $ compile cslCompiler
  match (fromGlob bibFile) $ compile biblioCompiler

publicationsCompiler :: Compiler (Item String)
publicationsCompiler = do
  csl <- load $ fromFilePath cslFile
  bib <- load $ fromFilePath bibFile
  body <- getResourceBody
  doc <- readPandocBiblio defaultHakyllReaderOptions csl bib body
  return $ writePandoc doc

cslFile :: String
cslFile = "publications/acm.csl"

bibFile :: String
bibFile = "publications/chungyc.bib"
