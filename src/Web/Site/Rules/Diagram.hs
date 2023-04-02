-- |
-- Description: Rules for generating images from Haskell code based on Diagrams.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
--
-- Exports the rules for generating images
-- from [diagrams](https://diagrams.github.io/)-based Haskell code.
module Web.Site.Rules.Diagram (rules) where

import Hakyll
import Web.Site.Compilers

-- |
-- Rules related to images generated from Haskell code based
-- on [diagrams](https://diagrams.github.io/).
--
-- The Haskell code will typically import the "Diagrams.Runner" module
-- for the 'Diagrams.Runner.putDiagram' helper function.
-- The generated image will be in SVG format.
--
-- For example, Haskell code generating an image could look like this:
--
-- @
--    import Diagrams.Runner
--
--    main :: IO ()
--    main = putDiagram defaultOptions roundThing
--
--    roundThing :: Diagram B
--    roundThing = circle 1 # lc red
-- @
--
-- If this code is in the file @site\/diagrams\/round.hs@,
-- this will generate the file @\/diagrams\/round.svg@ for the site.
rules :: Rules ()
rules = do
  match ("diagrams/**.hs" .||. "diagrams/**.lhs") $ do
    route $ setExtension "svg"
    compile $ haskellCompiler ["-XNoMonomorphismRestriction", "-XTypeFamilies"]
