---
title: Clean URLs with with Hakyll
published: 2023-02-06
updated: 2023-03-11
description: Another way to have URLs with no extensions for HTML pages with Hakyll.
toc: true
include-syntax-stylesheet: true
---

HTTP uses the [`Content-Type`] header to inform web browsers what they're getting back.
If we can control it, then it would be nicer to use a [clean URL] instead of a URL
that has an extension.  In other words, it would be nice if HTML pages did not
have `.html` in the URL.

However, by default [Hakyll] will always include the `.html` extension
with the HTML pages it generates.  It is actually not that hard to remove it, though,
and here I describe how I did it for my [personal web site](https://chungyc.org/).

[`Content-Type`]: https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Type
[clean URL]: https://en.wikipedia.org/wiki/Clean_URL
[Hakyll]: https://jaspervdj.be/hakyll/

## Setup in Hakyll

Some approaches rely on web servers typically serving the `index.html` file in a directory
to serve an HTML page corresponding to a URL for the directory.  However, I wanted to use
a more direct approach which avoided including `.html` in the file names in the first place.

With a vanilla installation of a Hakyll site, you will see code such as the following,
which switches the file name extension to `.html` for the file which will contain the
HTML output translated from the original file.

```haskell
match "about.markdown" $ do
  route $ setExtension "html"
  ...
```

It is really easy to switch things so that it removes the extension, instead.
Simply set the extension to the empty string instead of `.html`.

```haskell
match "about.markdown" $ do
  route $ setExtension ""
  ...
```

See [`src/Web/Site/Rules.hs`].

[`src/Web/Site/Rules.hs`]: https://github.com/chungyc/site-personal/blob/main/src/Web/Site/Rules.hs

## Setup in Apache

Using file names with no extension is all well and good, but it would be couterproductive
if web browsers treated the content as plain text or a blob of binary bytes.
In other words, we need the HTTP server to actually set the `Content-Type` to `text/html` for the HTML pages.

My web site is served using the Apache HTTP server.
Since I cannot change the main configuration for the server, I put the following in [`.htaccess`]:

```apache
<FilesMatch "^[^.]+$">
    ForceType text/html
</FilesMatch>
```

This will force the HTTP server to set the `Content-Type` to `text/html` if the file name has no extension.
Obviously, this will not work as intended if I had dots in the names of files containing HTML,
but this is fine for me because I have no such files, and my file naming convention avoids such files.

In fact, I have Hakyll generate my `.htaccess` file as well,
so I don't have to worry about copying or editing it separately.

See [`src/Web/Site/htaccess`].

[`.htaccess`]: https://httpd.apache.org/docs/2.4/howto/htaccess.html
[`src/Web/Site/htaccess`]: https://github.com/chungyc/site-personal/blob/main/site/server/htaccess

## Custom server

There is nothing more to do if all one wants is
to serve HTML pages without including the extension in the URL.
However, I would like to preview my site without standing up my own Apache HTTP server.

Hakyll uses the [warp] HTTP server for previewing a site locally.
It does not know to serve files without an extension as HTML,
so I made my own customizations to warp so that it would set `Content-Type` to `text/html`
for files without an extension.

```haskell
main = Warp.runSettings warpSettings $
  Static.staticApp baseSettings{ ssGetMimeType = getMimeType }
  where warpSettings = Warp.setHost (fromString "127.0.0.1") $
                         Warp.setPort 8000 Warp.defaultSettings
        baseSettings = Static.defaultFileServerSettings "_site"
        defaultGetMimeType = ssGetMimeType baseSettings
        getMimeType file =
          if '.' `elem` fromPiece (fileName file)
            then defaultGetMimeType file
            else return "text/html"
```

Some day I might propose changes to Hakyll so that it could pass
in such customizations to the HTTP server as an option.

See [`app/Server.hs`].

[warp]: https://hackage.haskell.org/package/warp
[`app/Server.hs`]: https://github.com/chungyc/site-personal/blob/main/app/Server.hs

## Caveats

There are a few caveats with the way I implemented clean URLs with Hakyll.

*   HTML files should not have a dot in their file names.

*   The link checker in Hakyll ignores files without the `.html` extension.
    This is not a problem for me because I use another tool to keep track of links.

## See also

The approach described on this page is not the only way to use clean URLs with Hakyll.
Others have described alternative approaches.

*   [Clean URLs with Hakyll](https://www.rohanjain.in/hakyll-clean-urls/) by Rohan Jain
*   [Jekyll Style URLs with Hakyll](http://aherrmann.github.io/programming/2016/01/31/jekyll-style-urls-with-hakyll/index.html) by Andreas Herrmann
