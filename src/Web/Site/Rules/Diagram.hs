-- |
-- Description: Rules for generating images from Haskell code based on Diagrams.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Web.Site.Rules.Diagram (rules) where

import Data.List (intercalate)
import Hakyll

-- import           Web.Site.Compilers

-- | Rules related to images generated from Haskell code based on Diagrams.
rules :: Rules ()
rules = do
  match "diagrams/**.hs" $ do
    route $ setExtension "svg"
    compile haskellCompiler'
  where
    haskellCompiler' = do
      getResourceString
        >>= return . fmap (\s -> concat [preamble, s, postamble])
        >>= withItemBody
          ( unixFilter
              "stack"
              [ "runhaskell",
                "--",
                "-XGHC2021",
                "-XOverloadedStrings"
              ]
          )

preamble :: String
preamble =
  intercalate
    "\n"
    [ "{-# LANGUAGE NoMonomorphismRestriction #-}",
      "{-# LANGUAGE TypeFamilies              #-}",
      "import           Data.Text.Lazy.IO    (putStr)",
      "import           Diagrams.Backend.SVG",
      "import           Diagrams.Prelude",
      "import           Graphics.Svg",
      "import           Prelude              hiding (putStr)",
      ""
    ]

postamble :: String
postamble = "\ndefaultOptions = SVGOptions (mkWidth 400) Nothing \"\" [] True"
