-- |
-- Description: Rules for generating stylesheets from Haskell code.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
--
-- Exports rules for generating the stylesheets for the web site.
module Web.Site.Rules.Stylesheet (rules) where

import Data.List (intercalate)
import Hakyll
import Text.Pandoc.Highlighting (pygments, styleToCss, zenburn)
import Web.Site.Compilers

-- |
-- Rules related to stylesheets generated from Haskell code.
--
-- Except for a few stylesheets which are not expected to change,
-- the stylesheets are generated from Haskell code via standard output.
-- While theoretically this can use any framework for generating
-- stylesheets, this web site uses [Clay](http://fvisser.nl/clay/).
rules :: Rules ()
rules = do
  match "css/**.hs" $ do
    route $ setExtension "css"
    compile $ getResourceLBS >>= haskellCompiler

  -- Stylesheet for supporting syntax highlighting.
  -- This will import the actual stylesheet according to the preferred color scheme.
  create ["css/syntax.css"] $ do
    route idRoute
    compile $
      makeItem $
        intercalate
          "\n"
          [ "@import \"syntax-light.css\" all and (prefers-color-scheme: light);",
            "@import \"syntax-dark.css\" all and (prefers-color-scheme: dark);",
            ""
          ]

  -- Syntax highlighting in light mode.
  create ["css/syntax-light.css"] $ do
    route idRoute
    compile $ makeItem $ styleToCss pygments

  -- Syntax highlighting in dark mode.
  create ["css/syntax-dark.css"] $ do
    route idRoute
    compile $ makeItem $ styleToCss zenburn
