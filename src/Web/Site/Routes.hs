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

-- $setup
-- >>> import Hakyll

-- |
-- Drop all file extensions from a route.
--
-- For example,
--
-- >>> let _ = route dropExtensions
dropExtensions :: Routes
dropExtensions = customRoute $ FilePath.dropExtensions . toFilePath
