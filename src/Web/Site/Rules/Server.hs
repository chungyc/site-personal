-- |
-- Description: Rules related to server setup.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Web.Site.Rules.Server (rules) where

import Hakyll

rules :: Rules ()
rules = do
  match "server/htaccess" $ do
    route $ constRoute ".htaccess"
    compile copyFileCompiler

  match "server/robots.txt" $ do
    route $ constRoute "robots.txt"
    compile copyFileCompiler

  match "server/favicon.gif" $ do
    route $ constRoute "favicon.gif"
    compile copyFileCompiler

  match "server/favicon.ico" $ do
    route $ constRoute "favicon.ico"
    compile copyFileCompiler

  match "server/errors/*.html" $ do
    route idRoute
    compile $
      getResourceBody
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
