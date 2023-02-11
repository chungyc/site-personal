-- |
-- Description: Functions related to Hakyll compilers used by other modules for this website.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Web.Site.Compilers (pandocCompilerWithMath) where

import Hakyll
import Text.Pandoc.Options

-- |
-- The Pandoc compiler, but with math support enabled.
pandocCompilerWithMath :: Compiler (Item String)
pandocCompilerWithMath = pandocCompilerWith mathReaderOptions mathWriterOptions

-- |
-- Reader options for properly treating math in input.
mathReaderOptions :: ReaderOptions
mathReaderOptions =
  defaultHakyllReaderOptions
    { readerExtensions =
        readerExtensions defaultHakyllReaderOptions
          <> extensionsFromList
            [ Ext_tex_math_single_backslash,
              Ext_tex_math_double_backslash,
              Ext_tex_math_dollars,
              Ext_latex_macros
            ]
    }

-- |
-- Writer options for writing out math to HTML.
--
-- We use KaTeX to render math, but the auto-render extension depends
-- on how Pandoc writes out math in MathJax.  It does not work with
-- how Pandoc writes out math in KaTeX.
mathWriterOptions :: WriterOptions
mathWriterOptions = defaultHakyllWriterOptions {writerHTMLMathMethod = MathJax ""}
