This page describes how the extra styles for the front page of this site are generated.

\subsection{Preamble}

As usual, we start with defining the main module, module imports,
and writing the CSS stylesheet to standard output.

\begin{code}
module Main (main) where

import Clay
import Clay.Media qualified as Media

main :: IO ()
main = putCss frontStyle
\end{code}

\subsection{Latest updates}

The front page may optionally include the latest \href{/updates}{update}.
It will style it a little bit differently than an update normally would be.
This is so that the update will not be confused with the rest of the front page.
It is this extra bit of styling that is done for the front page.

\begin{code}
-- |
-- Style for front page which Clay will render into a stylesheet.
frontStyle :: Css
frontStyle = do
  ".latest-update" ? do
    sym padding $ em 1

  query Media.all [Media.minWidth $ cm 10] $ do
    ".latest-update" ? do
      sym margin $ em 2
      sym padding $ em 2

  query Media.all [Media.prefersColorScheme Media.light] $ do
    ".latest-update" ? do
      border (px 2) solid $ rgb 20 20 50
      backgroundColor $ rgb 250 250 250

  query Media.all [Media.prefersColorScheme Media.dark] $ do
    ".latest-update" ? do
      border (px 2) solid $ rgb 230 230 200
      backgroundColor $ rgb 5 5 5
\end{code}

\subsection{See also}

\begin{itemize}
\item \href{https://github.com/chungyc/site-personal/blob/main/site/css/front.lhs}{Source code}
\item \href{/css/front.css}{Generated stylesheet}
\end{itemize}
