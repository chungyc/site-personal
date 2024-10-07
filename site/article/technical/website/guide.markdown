---
title: Guide to writing for this web site
description: Various knobs that are useful to writing pages for this web site.
published: 2023-03-18
updated: 2024-10-07
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

`head-extra`
:   Extra content to include in the [`head`] HTML element.

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

[`head`]: https://developer.mozilla.org/en-US/docs/Web/HTML/Element/head
[KaTeX]: https://katex.org/
[robots `meta` tag]: https://www.robotstxt.org/meta.html
[RSS feed]: https://validator.w3.org/feed/docs/rss2.html

### Front page

The front page will define the `front` metadata field.

The front page can also define a `include-latest-update` metadata field.
If defined, it will include the latest [update] on the front page.

[update]:  /updates

## Style

These are particular CSS elements I may need to use manually.

`keep-colors`
:   CSS class used with SVG images, for those whose colors should never invert on dark mode.
    The class should be associated with the `img` element inside a `figure` element;
    this is what the conversion to HTML from Markdown does.

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
:   Contains files related to generating stylesheets.
    They may be Haskell code for generating the stylesheets,
    or be CSS stylesheets themselves that are copied verbatim.

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

### Custom context

This site uses a custom [`siteContext`] instead of `defaultContext`.
It includes customizations to the default context specific to this site.
In particular, it strips `index.html` from directory URLs.

[`siteContext`]: https://chungyc.github.io/site-personal/Web-Site-Compilers.html#v:siteContext

### Generation from Haskell

[`haskellCompiler`] compiles items by running its input argument as Haskell code.
The output will be taken from the standard output of the executed Haskell code.

For example:

```haskell
compile $ haskellCompiler []
```

The code can be either Haskell or literate Haskell.

[`haskellCompiler`]: https://chungyc.github.io/site-personal/Web-Site-Compilers.html#v:haskellCompiler

### Math support

Use [`mathReaderWith`] and [`mathWriterWith`] to make Pandoc render math.

For example,

```haskell
let readerOptions = mathReaderWith defaultHakyllReaderOptions
let writerOptions = mathWriterWith defaultHakyllWriterOptions
compile $ pandocCompilerWith readerOptions writerOptions
```

[`mathReaderWith`]: https://chungyc.github.io/site-personal/Web-Site-Compilers.html#v:mathReaderWith
[`mathWriterWith`]: https://chungyc.github.io/site-personal/Web-Site-Compilers.html#v:mathWriterWith

### Table of contents

Use [`getTocOptionsWith`] to make Pandoc render a table of contents
if the `toc` metadata field is defined.
It is passed another Pandoc writer option as an input argument
so that it can be combined with other writer options.

[`getTocOptionsWith`]: https://chungyc.github.io/site-personal/Web-Site-Compilers.html#v:getTocOptionsWith

## Routes

The [`dropExtensions`] function strips all extensions from a route.
It is used for making URLs clean.

[`dropExtensions`]: https://chungyc.github.io/site-personal/Web-Site-Routes.html#v:dropExtensions

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

*   [Library documentation](https://chungyc.github.io/site-personal)
*   Source for the web site on [GitHub](https://github.com/chungyc/site-personal)
