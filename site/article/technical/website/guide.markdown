---
title: Guide to writing for this web site
description: Various knobs that are useful to writing pages for this web site.
published: 2023-03-18
toc: true
include-syntax-stylesheet: true
---

[Hakyll] is a static site generator written in [Haskell].
The rules for generating a site are written in Haskell,
and they can be customized with code.

The upshot is that Hakyll can be very flexible in the things it can be made to do,
which I have done with this site.
However, there are an increasing number of knobs that I have added
for generating the site, so I need them written down somewhere.
That somewhere is here.

[Hakyll]: https://jaspervdj.be/hakyll/
[Haskell]: https://haskell.org/

## Metadata

Hakyll metadata included for individual resources.

### Standard

The fields `title`, `description`, `published`, and `updated`
are standard Hakyll metadata fields with their usual meaning.
They should be included in page metadata when possible.

I use the sane and sensibly lexicographically sorted date format.
I.e., dates such as 2023-03-28.

### Custom

Custom metadata fields which can be defined for any page.

`extra-stylesheet`
:   A URL for an extra stylesheet to load for the page.
    For loading stylesheets only loaded by a few pages;
	other stylesheets will usually have dedicated metadata fields.

`include-math`
:   If defined, load [KaTeX] resources necessary for rendering math.

`include-bibliography-stylesheet`
:   If defined, load stylesheet responsible for bibliographic references.

`include-syntax-stylesheet`
:   If defined, load stylesheet responsible for syntax highlighting.

`robots`
:   Content for a [robots `meta` tag].

`rss-feed-link`
:   A URL for an [RSS feed].

`standalone-title`
:   If defined, the value for the `title` metadata field will be
    the entirety of the title for the page.  This is intended for the front page.
	Other pages are expected to have the site title included in the page title.

[KaTeX]: https://katex.org/
[robots `meta` tag]: https://www.robotstxt.org/meta.html
[RSS feed]: https://validator.w3.org/feed/docs/rss2.html

### Front page

The front page can define a `include-latest-update` metadata field.
If defined, it will include the latest [update] in the front page.

[update]:  /updates

## Directories

### Pages

`article/`
:   Generic pages which are not related to updates about me or the site.

`links/`
:   Links of interest.  These are basically public bookmarks.

`publications/`
:   List of publications by yours truly.

`update/`
:   Updates about me or the site.

### Support

`css/`
:   Haskell code for generating the stylesheets.

`diagrams/`
:   Haskell code for generating diagrams.

`files/`
:   Generic files to be included as is on the site.

`images/`
:   Images to be included as is on the site.

`server/`
:   Files related to the server infrastructure.
    E.g., Apache configuration.

`templates/`
:   Hakyll templates used by the site.

## Compiling

### Generation from Haskell

`haskellCompiler` compiles items by running its input argument as Haskell code.
The output will be taken from the standard output of the executed Haskell code.

A fully formed Haskell file can be compiled the following way:

```haskell
compile $ getResourceLBS >>= haskellCompiler
```

`haskellCompiler` takes an input argument instead of reading the resource body directly
so that it can be easy to transform the resource body into another form if needed.

For example,

```haskell
compile $ getResourceLBS >>= haskellCompiler . fmap (append preamble)
```

### Cleaning up URLs

This site uses [clean URLs].  For most pages, nothing further needs to be done
because they are routed to file names which serve as part of clean URLs in the first place.

However, some pages are routed to `index.html` files so that the directory can be the URL.
This is not an issue for manually inserted URLs, but URLs automatically collected,
such as the index of updates or the index of articles, will include the `index.html` string.
`cleanupIndexUrls` is used to clean up such URLs in the generated files.

For example,

```haskell
compile $
  pandocCompiler
    >>= loadAndApplyTemplate "templates/default.html" defaultContext
    >>= cleanupIndexUrls
```

[clean URLs]: /article/technical/website/extensionless

### Math support

Use `mathReaderOptions` and `mathWriterOptions` to make Pandoc render math.

For example,

```haskell
compile $ pandocCompilerWith mathReaderOptions mathWriterOptions
```

### Table of contents

Use `getTocOptionsWith` to make Pandoc render a table of contents
if the `toc` metadata field is defined.
It is passed another Pandoc writer option as an input argument
so that it can be combined with other writer options.

## Routes

The `stripExtension` function strips the extension from a route.
It is really just `setExtension ""`, but makes it more obvious
that it is stripping and not setting an extension.

## Rules

The modules under `Web.Site.Rules` define the rules for each portion of the web site.
I have a convention of exporting `rules` and `items` functions from such modules.

The `rules` function should be obvious.
It allows the central `Web.Site.Rules` module to call the rules in its sub-modules.

The `items` function returns the pattern which maps to resources
which should be included in the sitemap.
The `Web.Site.Rules.Sitemap` module uses them to collect the URLs
to include in the [sitemap] for the web site.

[sitemap]: https://www.sitemaps.org/

## See also

*   Source for the web site on [GitHub](https://github.com/chungyc/site-personal)
