---
title: URLs with no extensions using Hakyll
published: 2023-02-15
description: Another way to remove the requirement that the extension be included in URLs for HTML pages with Hakyll.
---

## Setup in Hakyll

```haskell
match "about.markdown" $ do
  route $ setExtension "html"
  ...
```

```haskell
match "about.markdown" $ do
  route $ setExtension ""
  ...
```

## Setup in Apache

```
<FilesMatch "^[^.]+$">
    ForceType text/html
</FilesMatch>
```

## Custom server

```haskell
main :: IO ()
main =
  Warp.runSettings warpSettings $
    Static.staticApp
      baseSettings
        { ssGetMimeType = getMimeType,
          ssMaxAge = MaxAgeSeconds 10,
          ss404Handler = Just missing
        }
  where
    warpSettings =
      Warp.setHost (fromString "127.0.0.1") $
        Warp.setPort 8000 Warp.defaultSettings

    baseSettings = Static.defaultFileServerSettings "_site"

    defaultGetMimeType = ssGetMimeType baseSettings

    -- Overrides MIME type for files with no extension
    -- so that HTML pages need no extension.
    getMimeType file =
      if hasExtension (fromPiece $ fileName file)
        then defaultGetMimeType file
        else return "text/html"

hasExtension :: Text -> Bool
hasExtension = Text.elem '.'
```

## Cons

## See also

The approach described on this page is not the only way
that URLs for HTML pages generated from Hakyll can avoid extensions.
Others have described alternative approaches.

*   [Clean URLs with Hakyll](https://www.rohanjain.in/hakyll-clean-urls/) by Rohan Jain
*   [Jekyll Style URLs with Hakyll](http://aherrmann.github.io/programming/2016/01/31/jekyll-style-urls-with-hakyll/index.html) by Andreas Herrmann
