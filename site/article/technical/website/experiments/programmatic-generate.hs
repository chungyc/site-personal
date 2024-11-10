module Main where

import Control.Monad (forM_)
import Data.ByteString.Lazy qualified as B
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 as H hiding (main)
import Text.Blaze.Html5.Attributes as A
import Web.Generate.Contents

main :: IO ()
main = B.putStr $ renderHtml $ do
  toc
    [ Entry "Counting numbers" "#count" [],
      Entry "Fibonacci numbers" "#fibonacci" [],
      Entry "Prime numbers" "#prime" []
    ]

  p $ do
    "This is content randomly generated from Haskell code.  "
    "It is an experiment with generating an article programmatically on this web site.  "
    "This page is not intended to be useful in general.  "
    "The source code for this page is on "
    a ! href "https://github.com/chungyc/site-personal/blob/main/site/article/technical/website/experiments/programmatic-generate.hs" $ "GitHub"
    ".  "

  h2 "Counting numbers" ! A.id "count"
  blockquote $ forM_ [1 .. 100 :: Int] $ (<> " ") . toHtml

  h2 "Fibonacci numbers" ! A.id "fibonacci"
  blockquote $ forM_ (take 100 fibonacci) $ (<> " ") . toHtml

  h2 "Prime numbers" ! A.id "prime"
  blockquote $ forM_ (take 100 primes) $ (<> " ") . toHtml

fibonacci :: [Integer]
fibonacci = 1 : 1 : [m + n | (m, n) <- zip fibonacci (drop 1 fibonacci)]

primes :: [Integer]
primes = 2 : [n | n <- [3, 5 ..], all (\m -> n `mod` m /= 0) (takeWhile (\m -> m * m <= n) primes)]
