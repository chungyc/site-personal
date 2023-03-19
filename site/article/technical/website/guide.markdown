---
title: Guide to writing for this web site
description: Various knobs that are useful to writing pages for this web site.
published: 2023-03-18
toc: true
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

TODO: A `include-bibliography-stylesheet` field should be added.

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

`haskellCompiler`

### Cleaning up URLs

`cleanupIndexUrls`

### Math support

`mathReaderOptions`

`mathWriterOptions`

### Table of contents

`getTocOptionsWith`

## Routes

`stripExtension`

## Rules

Convention of exporting `rules` and `items`.

## See also

*   Source on [GitHub](https://github.com/chungyc/site-personal)
