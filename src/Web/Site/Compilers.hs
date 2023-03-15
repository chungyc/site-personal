-- |
-- Description: Functions related to Hakyll compilers used by other modules for this website.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Web.Site.Compilers
  ( haskellCompiler,
    cleanupIndexUrls,
    mathReaderOptions,
    mathWriterOptions,
    getTocOptionsWith,
  )
where

import Hakyll
import Text.Pandoc (compileTemplate, runPure, runWithDefaultPartials)
import Text.Pandoc.Options

-- |
-- Run the content of the resource as Haskell code and use its output.
haskellCompiler :: Compiler (Item String)
haskellCompiler =
  getResourceString
    >>= withItemBody
      ( unixFilter
          "stack"
          [ "runhaskell",
            "--",
            "-XGHC2021",
            "-XOverloadedStrings"
          ]
      )

-- |
-- For local URLs in the input which end with @index.html@, strip it.
cleanupIndexUrls :: Item String -> Compiler (Item String)
cleanupIndexUrls = return . fmap (withUrls cleanupIndexUrl)

-- |
-- If the given URL is local and ends with @index.html@, strip the latter.
cleanupIndexUrl :: String -> String
cleanupIndexUrl url@('/' : _)
  | Nothing <- prefix = url
  | Just s <- prefix = s
  where
    prefix = needlePrefix "index.html" url
cleanupIndexUrl url = url

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

-- |
-- Rewrite the writer options to include a table of contents
-- if the source has a @toc@ field in its metadata.
-- If there is no such field, the given writer options are returned as is.
getTocOptionsWith :: WriterOptions -> Compiler WriterOptions
getTocOptionsWith options = do
  identifier <- getUnderlying
  tocField <- getMetadataField identifier "toc"
  return $ getOptions tocField
  where
    getOptions Nothing = options
    getOptions (Just _) =
      options
        { writerTableOfContents = True,
          writerTOCDepth = 4,
          writerTemplate = tocTemplate
        }
    tocTemplate
      | Right (Right t) <- build templateSource = Just t
      | otherwise = Nothing
    build = runPure . runWithDefaultPartials . compileTemplate ""
    templateSource = "<nav class='toc'><h2>Contents</h2>\n$toc$\n</nav>\n$body$"
