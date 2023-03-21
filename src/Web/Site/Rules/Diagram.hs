-- |
-- Description: Rules for generating images from Haskell code based on Diagrams.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
--
-- Exports the rules for generating images
-- from [diagrams](https://diagrams.github.io/)-based Haskell code.
module Web.Site.Rules.Diagram (rules) where

import Data.ByteString.Lazy
import Hakyll
import Web.Site.Compilers

-- |
-- Rules related to images generated from Haskell code based
-- on [diagrams](https://diagrams.github.io/).
--
-- The Haskell code should not have to import "Diagrams.Prelude" or 'Diagrams.Runner.putDiagram'.
-- These will be implicitly imported by prepending boilerplate code.
-- This allows the Haskell code for generating diagrams to only have
-- to define a main function which writes out the diagram, along
-- with any other functions necessary to generate the diagram.
-- The generated image will be in SVG format.
--
-- For example, Haskell code generating an image could look like this:
--
-- @
--    main = putDiagram defaultOptions roundThing
--
--    roundThing :: Diagram B
--    roundThing = circle 1 # lc red
-- @
--
-- If this code is in the file @site\/diagrams\/round.hs@,
-- this will generate the file @\/diagrams\/round.svg@ for the site.
--
-- Other modules can still be exported explicitly.  For example,
--
-- @
--     import Diagrams.TwoD.Path.IteratedSubset
--
--     main = putDiagram defaultOptions example
--
--     example :: Diagram B
--     example = strokeTrail $ snowflake 5
-- @
rules :: Rules ()
rules = do
  match "diagrams/**.hs" $ do
    route $ setExtension "svg"
    compile $ getResourceLBS >>= haskellCompiler . fmap (append preamble)

-- |
-- Common preamble to Diagrams-based code.
--
-- This will be used to remove common boilerplate from Haskell code
-- which are used to generate diagrams for this web site.
-- The preamble will import the "Diagrams.Runner" module,
-- which basically re-exports "Diagrams.Prelude".
-- It will also export the 'Diagrams.Runner.putDiagram' function,
-- which will be responsible for writing out a diagram in SVG format
-- to standard output.
--
-- This allows the Haskell code for generating diagrams to only have
-- to define a main function which writes out the diagram, along
-- with any other functions necessary to generate the diagram.
-- For example,
--
-- @
--    main = putDiagram defaultOptions roundThing
--
--    roundThing :: Diagram B
--    roundThing = circle 1 # lc red
-- @
preamble :: ByteString
preamble =
  intercalate
    "\n"
    [ "{-# LANGUAGE NoMonomorphismRestriction #-}",
      "{-# LANGUAGE TypeFamilies              #-}",
      "import Diagrams.Runner",
      ""
    ]
