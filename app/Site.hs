module Main (main) where

import Hakyll
import Web.Site (config, rules)

main :: IO ()
main = hakyllWith config rules
