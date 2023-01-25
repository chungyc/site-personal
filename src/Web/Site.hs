{-|
Description: Configuration and rules for generating the web site.
Copyright: Copyright (C) 2023 Yoo Chung
License: All rights reserved
Maintainer: yoo.chul.chung@gmail.com
-}
module Web.Site (config, rules) where

import           Hakyll
import           Web.Site.Rules (rules)

{-|
Configuration for Hakyll to generate the web site.
-}
config :: Configuration
config = defaultConfiguration { providerDirectory = "site" }
