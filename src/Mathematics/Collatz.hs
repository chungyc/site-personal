-- |
-- Description: Collatz sequences.
-- Copyright: Copyright (C) 2025 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
--
-- Functions related to
-- [Collatz sequences](https://www.quantamagazine.org/why-mathematicians-still-cant-solve-the-collatz-conjecture-20200922/)
-- and their properties.
--
-- Once a sequence reaches 1, we consider it the end of the sequence.
-- Strictly speaking, the sequence continues with the repeated sequence of 1, 4, 2, 1, 4, 2, ...,
-- but this repetition is not interesting.
module Mathematics.Collatz (next, collatz, collatzLength, collatzMax) where

import Data.Numbered

-- | Returns the next number in a Collatz sequence.
next :: Integer -> Integer
next n
  | even n = n `div` 2
  | otherwise = 3 * n + 1

-- | Returns a Collatz sequence starting from the given number.
collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz n = n : collatz (next n)

-- | Returns the length of a Collatz sequence starting from the given number.
collatzLength :: Integer -> Integer
collatzLength = find collatzLengths

-- | Maps @n@ to the length of the Collatz sequence starting from @n@.
collatzLengths :: Numbered Integer
collatzLengths = build f
  where
    f 1 = 1
    f n = 1 + find collatzLengths (next n)

-- | Returns the maximum element in a Collatz sequence starting from the given number.
collatzMax :: Integer -> Integer
collatzMax = find collatzMaxes

-- | Maps @n@ to the maximum element in the Collatz sequence starting from @n@.
collatzMaxes :: Numbered Integer
collatzMaxes = build f
  where
    f 1 = 1
    f n = max n $ find collatzMaxes (next n)
