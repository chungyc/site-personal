-- |
-- Description: Program for generating the web site.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Main (main) where

import Hakyll
import Web.Site (config, rules)

main :: IO ()
main = hakyllWith config rules
