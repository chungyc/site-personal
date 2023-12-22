This page describes how the extra styles for the \href{/about}{about page}
on this site are generated.

\subsection{Preamble}

As usual, we start with defining the main module, module imports,
and writing the CSS stylesheet to standard output.

\begin{code}
module Main (main) where

import Clay
import Clay.Media qualified as Media
import Prelude hiding (div)

main :: IO ()
main = putCss $ do
  profilePhotoStyle
\end{code}

\subsection{Profile photograph}

This styles the profile photograph on the about page.

First, we define which HTM element corresponds to the profile photograph.
The theshold for which the width is considered narrow or wide is also defined.

\begin{code}
profileElement :: Selector
profileElement = img # ".profile-photo"

widthThreshold :: Size LengthUnit
widthThreshold = em 30
\end{code}

For all cases, there is a bit of padding around the photograph.

\begin{code}
profilePhotoStyle :: Css
profilePhotoStyle = do
  profileElement ? do
    sym padding $ em 1

  wideProfilePhotoStyle
  narrowProfilePhotoStyle
\end{code}

When the width is large enough, the profile photograph
will float to the right and have a fixed size.

\begin{code}
wideProfilePhotoStyle :: Css
wideProfilePhotoStyle = do
  query Media.all [Media.minWidth widthThreshold] $ do
    profileElement ? do
      display inlineBlock
      float floatRight
      width $ em 8
      height $ em 8
\end{code}

When the width is narrow, the profile photograph will be in its own block and centered.

\begin{code}
narrowProfilePhotoStyle :: Css
narrowProfilePhotoStyle = do
  query Media.all [Media.maxWidth widthThreshold] $ do
    profileElement ? do
      display block
      float none
      marginLeft auto
      marginRight auto
      maxWidth $ pct 50
      height auto
      "aspect-ratio" -: "auto"
\end{code}

\subsection{See also}

\begin{itemize}
\item \href{https://github.com/chungyc/site-personal/blob/main/site/css/about.lhs}{Source code}
\item \href{/css/about.css}{Generated stylesheet}
\item \href{./}{Other stylesheets}
\end{itemize}
