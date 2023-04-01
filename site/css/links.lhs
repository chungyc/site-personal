This page describes how the extra styles for the link collection pages
on this site are generated.

\subsection{Preamble}

As usual, we start with defining the main module, module imports,
and writing the CSS stylesheet to standard output.

\begin{code}
module Main (main) where

import Clay
import Prelude hiding (div)

main :: IO ()
main = putCss linksStyle
\end{code}

\subsection{Link pages}

The main styling done for link collection pages is to reduce the heading sizes.
The default heading sizes are a bit large to fit naturally
in a collection of links.

\begin{code}
linksStyle :: Css
linksStyle = do
  h2 ? fontSize (em 1.4)
  h3 ? fontSize (em 1.3)
  h4 ? fontSize (em 1.2)
  h5 ? fontSize (em 1.1)
  h6 ? fontSize (em 1)
\end{code}

\subsection{See also}

\begin{itemize}
\item \href{https://github.com/chungyc/site-personal/blob/main/site/css/links.lhs}{Source code}
\item \href{/css/links.css}{Generated stylesheet}
\item \href{./}{Other stylesheets}
\end{itemize}
