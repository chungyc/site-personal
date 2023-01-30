-- |
-- Description: Hakyll routes used by other modules for this website.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Web.Site.Routes (stripExtension) where

import Hakyll

-- | Strip extension from the file name.
stripExtension :: Routes
stripExtension = setExtension ""
