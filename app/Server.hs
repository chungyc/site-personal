-- |
-- Description: Program for serving web site locally.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Main (main) where

import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Network.HTTP.Types.Status (status404)
import Network.Wai (Application, responseFile)
import Network.Wai.Application.Static qualified as Static
import Network.Wai.Handler.Warp qualified as Warp
import WaiAppStatic.Types

main :: IO ()
main =
  Warp.runSettings warpSettings $
    Static.staticApp
      baseSettings
        { ssGetMimeType = getMimeType,
          ssMaxAge = MaxAgeSeconds 10,
          ss404Handler = Just missing
        }
  where
    warpSettings =
      Warp.setHost (fromString "127.0.0.1") $
        Warp.setPort 8000 Warp.defaultSettings

    baseSettings = Static.defaultFileServerSettings "_site"

    defaultGetMimeType = ssGetMimeType baseSettings

    -- Overrides MIME type for files with no extension
    -- so that HTML pages need no extension.
    getMimeType file =
      if hasExtension (fromPiece $ fileName file)
        then defaultGetMimeType file
        else return "text/html"

-- | Whether a file name has an extension.
hasExtension :: Text -> Bool
hasExtension = Text.elem '.'

-- | Response handler for when missing resources are requested.
missing :: Application
missing _ respond = respond $ responseFile status404 headers file Nothing
  where
    headers = [("Content-Type", "text/html")]
    file = "_site/server/errors/missing.html"
