-- |
-- Description: Functions related to Hakyll compilers used by other modules for this website.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
--
-- Various Hakyll compilers and functions to assist use of Hakyll compilers.
module Web.Site.Compilers
  ( -- * Compilers
    haskellCompiler,

    -- * Post-processors
    cleanupIndexUrls,
    cleanupIndexUrl,

    -- * Pandoc options

    -- | Pandoc reader and writer options that can be used with 'pandocCompilerWith'.
    mathReaderOptions,
    mathWriterOptions,
    getTocOptionsWith,
  )
where

import Data.ByteString.Lazy (ByteString)
import Hakyll
import Text.Pandoc (compileTemplate, runPure, runWithDefaultPartials)
import Text.Pandoc.Options

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.ByteString.Lazy
-- >>> import Hakyll

-- |
-- Run the Haskell code in the underlying file and use its output.
--
-- For example,
--
-- >>> let _ = compile $ haskellCompiler []
--
-- The Haskell code will be executed using @runhaskell@.
-- It will run with @-XGHC2021@ and @-XOverloadedStrings@.
--
-- Extra flags can also be passed to @runhaskell@.
-- For example,
--
-- >>> let _ = compile $ haskellCompiler ["-XTypeFamilies"]
--
-- This can compile both Haskell code and literate Haskell code.
haskellCompiler ::
  -- | Extra flags to pass to @runhaskell@.
  [String] ->
  Compiler (Item ByteString)
haskellCompiler args = do
  file <- getResourceFilePath
  emptyItem >>= withItemBody (run file)
  where
    -- Run the Haskell code in the given file and return its standard output.
    run f = unixFilterLBS "runhaskell" $ concat [defaultArgs, args, [f]]

    -- Default flags to always use with @runhaskell@.
    defaultArgs = ["-XGHC2021", "-XOverloadedStrings"]

    -- We will run the code from the file directly,
    -- so we don't care about any content in an item.
    emptyItem = makeItem ""

-- |
-- For local URLs in the input which end with @index.html@, strip it.
--
-- For example:
--
-- >>> :{
-- let _ = pandocCompiler
--           >>= loadAndApplyTemplate "templates/default.html" defaultContext
--           >>= cleanupIndexUrls
-- :}
cleanupIndexUrls :: Item String -> Compiler (Item String)
cleanupIndexUrls = return . fmap (withUrls cleanupIndexUrl)

-- |
-- If the given URL is local and ends with @index.html@, strip the latter.
--
-- For example:
--
-- >>> cleanupIndexUrl "/article/index.html"
-- "/article/"
-- >>> cleanupIndexUrl "/article/page.html"
-- "/article/page.html"
-- >>> cleanupIndexUrl "http://chungyc.org/article/index.html"
-- "http://chungyc.org/article/index.html"
--
-- One will usually use 'cleanupIndexUrls' to clean up URLs in generated pages,
-- instead of using this function directly.
cleanupIndexUrl :: String -> String
cleanupIndexUrl url@('/' : _)
  | Nothing <- prefix = url
  | Just s <- prefix = s
  where
    prefix = needlePrefix "index.html" url
cleanupIndexUrl url = url

-- |
-- Reader options for properly treating math in input.
--
-- Should be used in conjunction with 'mathWriterOptions' for proper math rendering.
-- For example:
--
-- >>> let _ = pandocCompilerWith mathReaderOptions mathWriterOptions
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
-- Should be used in conjunction with 'mathReaderOptions'
-- to read input that is to be rendered as math.
-- For example:
--
-- >>> let _ = pandocCompilerWith mathReaderOptions mathWriterOptions
--
-- Pages which use math should define the @include-math@ metadata field
-- to ensure that the resources necessary for rendering math is included.
mathWriterOptions :: WriterOptions
mathWriterOptions =
  defaultHakyllWriterOptions
    { -- We use KaTeX to render math, but the auto-render extension depends
      -- on how Pandoc writes out math in MathJax.  It does not work with
      -- how Pandoc writes out math in KaTeX.
      writerHTMLMathMethod = MathJax ""
    }

-- |
-- Rewrite the writer options to include a table of contents
-- if the source has a @toc@ field in its metadata.
-- If there is no such field, the given writer options are returned as is.
--
-- For example:
--
-- >>> let _ = getTocOptionsWith defaultHakyllWriterOptions
-- >>> let _ = getTocOptionsWith mathWriterOptions
getTocOptionsWith :: WriterOptions -> Compiler WriterOptions
getTocOptionsWith options = do
  identifier <- getUnderlying
  tocField <- getMetadataField identifier "toc"
  return $ (getOptions tocField)
  where
    getOptions Nothing = options
    getOptions (Just _) =
      options
        { writerTableOfContents = True,
          writerTOCDepth = 3,
          writerTemplate = tocTemplate
        }

    -- Pandoc metadata is not Hakyll metadata,
    -- so Pandoc has to take care of writing out the table of contents,
    -- instead of Hakyll being able to write it out with its own templates.
    tocTemplate
      | Right (Right t) <- build templateSource = Just t
      | otherwise = Nothing
    build = runPure . runWithDefaultPartials . compileTemplate ""
    templateSource = "<nav class='toc'><h2>Contents</h2>\n$toc$\n</nav>\n$body$"
