-- |
-- Description: Configuration and rules for generating the web site.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Web.Site (config, rules) where

import Hakyll
import Web.Site.Rules (rules)

-- |
-- Configuration for Hakyll to generate the web site.
config :: Configuration
config =
  defaultConfiguration
    { providerDirectory = "site",
      deployCommand =
        unwords
          [ "rsync",
            "-avz",
            "--checksum",
            "--delete",
            "--exclude .well-known",
            "_site/",
            "chungyc@chungyc.org:chungyc.org/"
          ]
    }
