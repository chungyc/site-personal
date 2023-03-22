-- |
-- Description: Hakyll routes used by other modules for this website.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
--
-- Hakyll route functions useful for this web site.
module Web.Site.Routes (dropExtensions) where

import Hakyll
import System.FilePath qualified as FilePath

-- |
-- Strip all extensions from a route.
dropExtensions :: Routes
dropExtensions = customRoute $ FilePath.dropExtensions . toFilePath
