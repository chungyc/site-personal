---
title: Literate Haskell experiment
description: Page for experimenting with use of literate Haskell to generate a page on the site.
published: 2023-03-22
toc: true
include-math: true
include-syntax-stylesheet: true
---

It's nice to be able to mix serious writing and coding together.
While [literate Haskell] may be far from the pinnacle of literate programming,
it is still a nice tool to have available.

This is a page for experimenting with its use.

[literate Haskell]: https://wiki.haskell.org/Literate_programming

Fibonacci numbers
-----------------

Say I want a list of Fibonacci numbers.
This is going to be a list of integers.

> fibonacci :: [Integer]

The first and second Fibonacci numbers are both defined to be 1,
so the list should start with two 1s.
We will define a list which will start out our list of Fibonacci numbers.

> base :: [Integer]
> base = [1, 1]

Subsequent Fibonacci numbers are defined by adding the two numbers
right before in the sequence.
We gather a list of two consecutive numbers, which we will add later together.

> pairs :: [(Integer,Integer)]
> pairs = zip fibonacci $ drop 1 fibonacci

Then getting the subsequent Fibonacci numbers is a simple matter of adding
together the pairs of numbers in the list above.

> inducted :: [Integer]
> inducted = map (uncurry (+)) pairs

Now that we have all the Fibonacci numbers in two lists,
we can simply concatenate the them together to obtain the entire list.

> fibonacci = base ++ inducted

And that's it.  We now have a list of all the Fibonacci numbers.

It is also possible to define the same thing with a single list.
Personally, I think this style is a little bit too clever,
given that it can make it awkward to write down clarifying comments
for various parts of the code.

> fibonacci' :: [Integer]
> fibonacci' = 1 : 1 : [ m+n | (m,n) <- zip fibonacci (drop 1 fibonacci) ]

We obviously cannot print the entirety of the list, but we can look up a few numbers.

> main :: IO ()
> main = do
>   print $ take 10 fibonacci
>   print $ take 11 fibonacci'

If we run the code above, it will print out the Fibonacci numbers as expected.

 > ```
 > $ runhaskell literate-haskell.lhs
 > [1,1,2,3,5,8,13,21,34,55]
 > [1,1,2,3,5,8,13,21,34,55,89]
 > ```

Despite these being infinite lists, defined in a somewhat more typically mathematical style,
the code is not stuck trying to futilely compute all the numbers before printing
any of them out thanks to lazy evaluation in Haskell.

Source
------

The raw source code for this file is on
[GitHub](https://github.com/chungyc/site-personal/tree/main/site/article/technical/website/experiments/literate-haskell.lhs)
