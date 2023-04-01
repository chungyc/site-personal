This page describes how the extra styles for error pages
on this site are generated.

\subsection{Preamble}

As usual, we start with defining the main module, module imports,
and writing the CSS stylesheet to standard output.

\begin{code}
module Main (main) where

import Clay
import Prelude hiding (div)

main :: IO ()
main = putCss errorStyle
\end{code}

\subsection{Errors}

The extra styling for error pages is basically to make a big letter class,
so that single characters can be used like a big image all by itself.
It makes error pages such as a 404 status page a little less boring
than just having a single sentence explaining the error.

\begin{code}
errorStyle :: Css
errorStyle = do
  div # ".letter-image" ? fontSize (em 20)
\end{code}

\subsection{See also}

\begin{itemize}
\item \href{https://github.com/chungyc/site-personal/blob/main/site/css/error.lhs}{Source code}
\item \href{/css/error.css}{Generated stylesheet}
\item \href{./}{Other stylesheets}
\end{itemize}
