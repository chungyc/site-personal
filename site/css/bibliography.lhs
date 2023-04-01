This page describes how the extra styles for bibliographic references
on this site are generated.

\subsection{Preamble}

As usual, we start with defining the main module, module imports,
and writing the CSS stylesheet to standard output.

\begin{code}
module Main (main) where

import Clay
import Prelude hiding (div, not)

main :: IO ()
main = putCss biblioStyle
\end{code}

\subsection{Bibliography}

This site uses the
\href{https://github.com/citation-style-language/styles/blob/master/association-for-computing-machinery.csl}{citation style}
used by the
\href{https://www.acm.org/}{Association for Computing Machinery}.
The stylesheet here is responsible for ensuring that the citation keys
are lined up and that there is appropriate spacing between entries.

\begin{code}
biblioStyle :: Css
biblioStyle = do
  div # ".csl-bib-body" ? do
    paddingBottom $ em 1

    div # ".csl-entry" ? do
      marginBottom $ em 1

      -- When width is small, break long URLs.
      overflowWrap breakWord

      div # ".csl-left-margin" ? do
        float floatLeft
        width $ em 3
        textAlign $ alignSide sideRight

      div # ".csl-right-inline" ? do
        marginLeft $ em 3
        paddingLeft $ em 1
\end{code}

\subsection{See also}

\begin{itemize}
\item \href{https://github.com/chungyc/site-personal/blob/main/site/css/bibliography.lhs}{Source code}
\item \href{/css/bibliography.css}{Generated stylesheet}
\end{itemize}
