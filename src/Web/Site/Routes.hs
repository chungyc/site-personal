-- |
-- Description: Hakyll routes used by other modules for this website.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
--
-- Hakyll route functions useful for this web site.
module Web.Site.Routes (stripExtension) where

import Hakyll

-- |
-- Strip extension from the file name.
--
-- It is the same as @setExtension ""@, but just with a more obvious name.
stripExtension :: Routes
stripExtension = setExtension ""
