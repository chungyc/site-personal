This page explains how the default stylesheet used by all pages on this site is defined.
It is Haskell code using \href{http://fvisser.nl/clay/}{Clay} to generate the CSS stylesheet.

\subsection{Preamble}

As with all Haskell applications, it starts out with defining the main module
and the modules it imports.

\begin{code}
module Main (main) where

import Clay
import Clay.Media qualified as Media
\end{code}

A few function names from the Prelude module conflict with HTML element names or CSS keywords.
It is more convenient to use the HTML or CSS names directly,
so we hide the conflicting names from the Prelude.

\begin{code}
import Prelude hiding (div, filter, not, (**))
\end{code}

And of course, we should output our generated CSS stylesheet.

\begin{code}
main :: IO ()
main = putCss defaultStyle
\end{code}

\subsection{Everything}

Next, we define the high-level function which calls all the other functons
generating the stylesheet.

\begin{code}
defaultStyle :: Css
defaultStyle = do
\end{code}

We want the generic styles to be generated first,
because they should be overridden by later styles in specific contexts.

\begin{code}
  genericStyle
\end{code}

We then generate the styles which are only relevant to specific contexts.

\begin{code}
  codeStyle
  articleStyle
  tableOfContents
  figures
\end{code}

Finally, the media-specific styles should be generated last.
They may need to override previously generated styles
so that they are more appropriate to specific media
such as narrower screens.

\begin{code}
  mediaStyles
\end{code}

\subsection{Generic styles}

These are styles which apply generally.
In other words, if there are no styles which need to apply more specifically in a certain context,
these are the styles which are applied.

\begin{code}
genericStyle :: Css
genericStyle = do
\end{code}

I basically chose the default fonts based on my arbitrary preferences.
The only condition I had was that they are expected to be available in any browser.
Legibility also takes precedence over anything else such as looking prettier.
I can't stand jagged edges in text, so I specify that text be justified.

\begin{code}
  html ? do
    fontFamily ["Georgia", "Garamond"] [serif, sansSerif, monospace]
    textRendering optimizeLegibility
    textAlign justify
    lineHeight $ unitless 1.25
\end{code}

In narrow screens, it often feels like there is a great waste of empty space
without words being hyphenated as appropriate, so automatic hyphenation is enabled.
I think hyphenation is fine in general, so I don't restrict its use to only narrow screens.

\begin{code}
    hyphens auto
\end{code}

The rest of the generic styles were basically tweaked by me
until they achieved a minimal level of acceptability.
Unexpectedly, I ended up liking the minimal look,
so I will probably stick with it.

\begin{code}
  body ? do
    marginTop $ em 2
    marginLeft $ pct 10
    marginRight $ pct 10
    marginBottom $ em 2

  headings

  footer ? do
    fontFamily ["Courier New"] [monospace, sansSerif]
    fontSize $ em 0.75
    borderTop (px 1) solid black
    marginTop $ em 1

  footer |> nav ? do
    paddingTop $ em 1.5
    a ? paddingRight (em 1)

  dt ? do
    fontWeight bold
    marginBottom $ em 0.25

  dd ? do
    marginBottom $ em 1
\end{code}

One thing to note is that I want some spacing between list items.
So there is a margin between each list item.
There is only an extra margin for a list if it is a direct child of a list item,
since there would be extra margins from other block elements
if the list was not a direct child.

\begin{code}
  li |+ li ? marginTop (em 0.75)

  li |> (ul <> ol) ? marginTop (em 0.75)
\end{code}

\subsubsection{Heading styles}

For the sole reason that I use monospace fonts with code,
I have the headings in a monospace font as well.

Except for the first-level heading which basically serves as the displayed title for the page,
I add dotted underlines to distinguish the headings from normal text.
Different levels of headings are distinguished by different sizes;
the different sizes may be a bit too subtle, but I'm not sure what to do about it.
At least the heading levels will be obvious in the table of contents.
Later, heading levels will also be distinguished by fading colors,
when the colors are specified for each preferred color scheme for specific media.

\begin{code}
headings :: Css
headings = do
  h1 ? do
    fontFamily ["Courier New"] [monospace, sansSerif]
    fontSize $ em 2
    fontStyle italic

  h2 <> h3 <> h4 <> h5 <> h6 ? do
    fontFamily ["Courier New"] [monospace, sansSerif]
    textDecorationLine underline
    textDecorationStyle dotted

  h2 ? fontSize (em 1.8)
  h3 ? fontSize (em 1.5)
  h4 ? fontSize (em 1.25)
  h5 ? fontSize (em 1.1)
\end{code}

\subsection{Content-specific styles}

\subsubsection{Code}

Style for code snippets.

\begin{code}
codeStyle :: Css
codeStyle = do
  div # ".sourceCode" ? do
    borderStyle solid
    borderWidth $ px 1
    marginRight $ em 1
    marginLeft $ em 1
    sym padding $ em 0.5
\end{code}

\subsubsection{Articles}

Style for \href{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/article}{\texttt{article}} elements.

\begin{code}
articleStyle :: Css
articleStyle = do
  article |> section # ".byline" ? do
    fontFamily ["Verdana"] [sansSerif, serif, monospace]
    fontSize $ em 0.7
    p ? do
      marginTop $ em 0.2
      marginBottom $ em 0.2
\end{code}

\subsubsection{Table of contents}

Style for table of contents.

\begin{code}
tableOfContents :: Css
tableOfContents = do
  nav # ".toc" ? do
    marginTop $ em 1
    marginBottom $ em 1
    sym padding $ em 1
    borderStyle solid
    borderWidth $ px 1

    h2 ? do
      fontFamily ["Georgia", "Garamond"] [serif, sansSerif, monospace]
      fontSize $ em 1.2
      fontStyle normal
      fontWeight bold
      textDecorationLine none
      textDecorationStyle none
      marginTop $ em 0.1
      marginBottom $ em 0.25

    ul <> (ul ** (ul <> li)) ? do
      paddingLeft $ em 0.75
      marginTop $ em 0.1
      marginBottom $ em 0.1
      listStyleType none
\end{code}

\subsubsection{Images}

Style for images.

\begin{code}
figures :: Css
figures = do
  figure ? do
    display block
    paddingTop $ em 0.5
    paddingBottom $ em 0.5
    marginTop $ em 1
    marginBottom $ em 1
    marginLeft auto
    marginRight auto
    textAlign center

    img ? do
      display block
      marginLeft auto
      marginRight auto
      maxWidth $ pct 95

    figcaption ? do
      display block
      fontFamily ["Verdana"] [sansSerif, serif, monospace]
      fontSize $ em 0.9
      marginTop $ em 0.5
      marginLeft auto
      marginRight auto
\end{code}

\subsection{Media-specific styles}

If the width is too narrow, we reduce the margins and paddings.
Conversely, if the width is too wide, we limit the width of a page
so that we do not have horrendously wide walls of text.
We also set the colors depending on whether the preferred color scheme
is light mode or dark mode.

\begin{code}
mediaStyles :: Css
mediaStyles = do
  query Media.all [Media.maxWidth $ em 30] $ do
    body ? sym margin (em 1)

    ul <> ol ? do
      marginLeft $ em 0.5
      paddingLeft $ em 0.5

  query Media.all [Media.minWidth $ em 60] $ do
    body ? do
      width $ em 60
      marginRight auto
      marginLeft auto

  query Media.all [Media.prefersColorScheme Media.light] lightColorScheme

  query Media.all [Media.prefersColorScheme Media.dark] darkColorScheme
\end{code}

We define this function to map each heading level to a color,
which makes it convenient to define heading colors
when setting the colors programmatically based on the heading level.

\begin{code}
headingColors :: (Integer -> Color) -> Css
headingColors mapColor = do
  h1 ? fontColor (mapColor 1)
  h2 ? fontColor (mapColor 2)
  h3 ? fontColor (mapColor 3)
  h4 ? fontColor (mapColor 4)
  h5 ? fontColor (mapColor 5)
  h6 ? fontColor (mapColor 6)
\end{code}

\subsubsection{Light mode}

Color scheme to use in light mode.

\begin{code}
lightColorScheme :: Css
lightColorScheme = do
  html ? do
    color black
    backgroundColor white

  a # link ? color blue
  a # visited ? color purple

  headingColors headingColor

  nav # ".toc" ? do
    borderColor lightgrey
    backgroundColor $ rgb 240 240 240

  div # ".sourceCode" ? borderColor lightgrey

  where
    headingColor n = rgb (n * 20) (n * 20) (100 + n * 10)
\end{code}

\subsubsection{Dark mode}

Color scheme to use in dark mode.

\begin{code}
darkColorScheme :: Css
darkColorScheme = do
  html ? do
    color white
    backgroundColor black

  footer ? borderTop (px 1) solid white

  a # link ? color cyan
  a # visited ? color pink

  headingColors headingColor

  nav # ".toc" ? do
    borderColor dimgrey
    backgroundColor $ rgb 20 20 20

  figure |> img # (not ".keep-colors" <> "src" $= ".svg") ? do
    filter (invert $ pct 100)

  div # ".sourceCode" ? borderColor dimgrey

  where
    headingColor n = rgb (255 - n * 20) (255 - n * 20) (155 - n * 10)
\end{code}

\subsection{See also}

\begin{itemize}
\item \href{https://github.com/chungyc/site-personal/blob/main/site/css/default.lhs}{Source code}
\item \href{/css/default.css}{Generated stylesheet}
\item \href{./}{Other stylesheets}
\end{itemize}
