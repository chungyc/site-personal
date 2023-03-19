-- |
-- Description: Configuration and rules for generating the web site.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Web.Site (config, rules) where

import Data.Text qualified as Text
import Hakyll
import Network.HTTP.Types.Status (status404)
import Network.Wai (Application, responseFile)
import Network.Wai.Application.Static qualified as Static
import System.FilePath (takeExtension)
import WaiAppStatic.Types
import Web.Site.Rules (rules)

-- | Configuration for Hakyll to generate the web site.
config :: Configuration
config =
  defaultConfiguration
    { providerDirectory = "site",
      checkHtmlFile = hasExtension,
      deployCommand =
        unwords
          [ "rsync",
            "-avz",
            "--checksum",
            "--delete",
            "--exclude .well-known",
            "_site/",
            "chungyc@chungyc.org:chungyc.org/"
          ],
      previewSettings = serverSettings
    }

-- | Whether a file name has an extension.
hasExtension :: FilePath -> Bool
hasExtension path
  | "" <- extension = True
  | "html" <- extension = True
  | otherwise = False
  where
    extension = takeExtension path

-- | Server customizations which fit better with this web site.
serverSettings :: FilePath -> Static.StaticSettings
serverSettings path =
  baseSettings
    { -- Allow HTML files to lack an extension.
      ssGetMimeType = getMimeType,
      -- Prevent browsers from caching files which may have changed for too long.
      ssMaxAge = MaxAgeSeconds 10,
      -- A 404 page consistent with the rest of the site is much better.
      ss404Handler = Just missing
    }
  where
    baseSettings = Static.defaultFileServerSettings path
    defaultGetMimeType = ssGetMimeType baseSettings

    -- Overrides MIME type for files with no extension
    -- so that HTML pages need no extension.
    getMimeType file =
      if Text.elem '.' (fromPiece $ fileName file)
        then defaultGetMimeType file
        else return "text/html"

-- | Response handler for when missing resources are requested.
missing :: Application
missing _ respond = respond $ responseFile status404 headers file Nothing
  where
    headers = [("Content-Type", "text/html")]
    file = "_site/server/errors/missing.html"
