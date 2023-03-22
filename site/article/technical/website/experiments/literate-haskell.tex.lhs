It's nice to be able to mix serious writing and coding together.
While \href{https://wiki.haskell.org/Literate_programming}{literate Haskell}
may be far from the pinnacle of literate programming,
it is still a nice tool to have available.

This is a page for experimenting with its use.

\subsection{Fibonacci numbers}

Say I want a list of Fibonacci numbers.
This is going to be a list of integers.

\begin{code}
fibonacci :: [Integer]
\end{code}

The first and second Fibonacci numbers are both defined to be 1,
so the list should start with two 1s.
We will define a list which will start out our list of Fibonacci numbers.

\begin{code}
base :: [Integer]
base = [1, 1]
\end{code}

Subsequent Fibonacci numbers are defined by adding the two numbers
right before in the sequence.
We gather a list of two consecutive numbers, which we will add later together.

\begin{code}
pairs :: [(Integer,Integer)]
pairs = zip fibonacci $ drop 1 fibonacci
\end{code}

Then getting the subsequent Fibonacci numbers is a simple matter of adding
together the pairs of numbers in the list above.

\begin{code}
inducted :: [Integer]
inducted = map (uncurry (+)) pairs
\end{code}

Now that we have all the Fibonacci numbers in two lists,
we can simply concatenate the them together to obtain the entire list.

\begin{code}
fibonacci = base ++ inducted
\end{code}

And that's it.  We now have a list of all the Fibonacci numbers.

It is also possible to define the same thing with a single list.
Personally, I think this style is a little bit too clever,
given that it can make it awkward to write down clarifying comments
for various parts of the code.

\begin{code}
fibonacci' :: [Integer]
fibonacci' = 1 : 1 : [ m+n | (m,n) <- zip fibonacci (drop 1 fibonacci) ]
\end{code}

We obviously cannot print the entirety of the list, but we can look up a few numbers.

\begin{code}
main :: IO ()
main = do
  print $ take 10 fibonacci
  print $ take 11 fibonacci'
\end{code}

If we run the code above, it will print out the Fibonacci numbers as expected.

\begin{verbatim}
$ runhaskell literate-haskell.lhs
[1,1,2,3,5,8,13,21,34,55]
[1,1,2,3,5,8,13,21,34,55,89]
\end{verbatim}

Despite these being infinite lists, defined in a somewhat more typically mathematical style,
the code is not stuck trying to futilely compute all the numbers before printing
any of them out thanks to lazy evaluation in Haskell.

\subsection{Source}

The raw source code for this file is on
\href{https://github.com/chungyc/site-personal/tree/main/site/article/technical/website/experiments/literate-haskell.tex.lhs}{GitHub}.
