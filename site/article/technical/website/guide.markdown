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

### Standard

`title`

`description`

`published`

`updated`

### Custom

`standalone-title`

`robots`

`include-syntax-stylesheet`

`extra-stylesheet`

`rss-feed-link`

`include-math`

### Front page

`include-latest-update`

## Directories

### Pages

`article`

`links`

`publications`

`update`

### Support

`css`

`diagrams`

`files`

`images`

`server`

`templates`

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
