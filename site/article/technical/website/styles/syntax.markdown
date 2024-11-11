---
title: Supporting syntax highlighting in Hakyll
description: How to generate stylesheets which support syntax highlighting provided by Hakyll through Pandoc.
published: 2024-11-11
toc: true
include-syntax-stylesheet: true
---

The FAQ for Hakyll suggests [copying a default syntax stylesheet][hakyll-syntax-suggestion]
to support syntax highlighting in code blocks.  If you are happy with the syntax highlighting
provided by this stylesheet, then this is all you have to do.
However, if you want to copy the stylesheet from a canonical source,
or if you want to use another syntax highlighting style provided by Pandoc,
you will not be able to find canonical stylesheet files from the original source.

[hakyll-syntax-suggestion]: https://jaspervdj.be/hakyll/tutorials/faq.html#does-hakyll-support-syntax-highlighting

## Generating stylesheets {#generate}

The reason that you will not find a stylesheet file from the original source,
which is the [`skylighting`] library, is that it does not define the styles
in CSS files.  Instead, it defines the styles in Haskell, and CSS files
are generated from these styles.

[`skylighting`]: https://github.com/jgm/skylighting

To generate the stylesheet supporting syntax highlighting for Hakyll,
you can define a rule which generates the CSS file
from a syntax highlighting style.  For example,

```haskell
import Hakyll
import Text.Pandoc.Highlighting (pygments, styleToCss)

rules :: Rules ()
rules = do
  create ["css/syntax.css"] $ do
    route idRoute
    compile $ makeItem $ styleToCss pygments
```

With the stylesheet in hand, it can be used by linking it from the
`<head>` element in HTML.

```html
<link rel="stylesheet" href="/css/syntax.css" />
```

While the example above used the `pygments` syntax highlighting style,
Pandoc provides [other styles][pandoc-syntax-styles] as well.

[pandoc-syntax-styles]: https://hackage.haskell.org/package/pandoc-3.5/docs/Text-Pandoc-Highlighting.html#g:5

## Supporting dark mode {#color-scheme-support}

You may actually use two separate syntax highlighting styles for different color schemes.
One would be used for light mode and the other for dark mode.
To do so, a wrapper stylesheet can be added which would import
different stylesheets depending on the color scheme.

In the example below, the `pygments` style would be used for light mode
and the `zenburn` style would be used for dark mode.[^wrapper-stylesheet]

[^wrapper-stylesheet]: The wrapper stylesheet is generated instead of
  copied from a file to make it easy to see that the imported files match
  the files which are generated.
  This is my preference, but there is no fundamental reason the wrapper
  stylesheet could not have simply been a CSS file that is copied.

```haskell
import Data.List (intercalate)
import Hakyll
import Text.Pandoc.Highlighting (pygments, styleToCss, zenburn)

rules :: Rules ()
rules = do
  -- Stylesheet for supporting syntax highlighting.
  -- This will import the actual stylesheet according to the preferred color scheme.
  create ["css/syntax.css"] $ do
    route idRoute
    compile $
      makeItem $
        intercalate
          "\n"
          [ "@import \"syntax-light.css\" all and (prefers-color-scheme: light);",
            "@import \"syntax-dark.css\" all and (prefers-color-scheme: dark);",
            ""
          ]

  -- Syntax highlighting in light mode.
  create ["css/syntax-light.css"] $ do
    route idRoute
    compile $ makeItem $ styleToCss pygments

  -- Syntax highlighting in dark mode.
  create ["css/syntax-dark.css"] $ do
    route idRoute
    compile $ makeItem $ styleToCss zenburn
```

You can then link the wrapper stylesheet in the `<head>` element.

```html
<link rel="stylesheet" href="/css/syntax.css" />
```

Switching the syntax highlighting style depending on color scheme was actually
the main reason I didn't want to go with the simple approach of copying the
suggested stylesheet from the Hakyll FAQ.  It was terrible when used in dark mode,
which led me to investigate how I can get alternate syntax highlighting styles
short of writing a stylesheet myself.

## See also {#refs}

*   [Source code] for generating the stylesheets for this web site.

*   The same approach is also [explained by Rebecca Skinner][rskinner].

[Source code]: https://github.com/chungyc/site-personal/blob/main/src/Web/Site/Rules/Stylesheet.hs

[rskinner]: https://rebeccaskinner.net/posts/2021-01-31-hakyll-syntax-highlighting.html
