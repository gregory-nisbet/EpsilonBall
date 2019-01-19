> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE TypeSynonymInstances #-}

> module EpsilonBall where

> import Data.Ratio (Rational, numerator, denominator)
> import qualified Data.Map as Map
> import Numeric.Natural (Natural)

EBInterval

An interval is logically a container, but its contents cannot be walked.
Another good name for this concept would be "set".

> class UntraversableContainer v a where
>   contains :: v -> a -> Bool


Addable is essentially an Abelian group, but has more semantic content

> class Addable a where
>   add :: a -> a -> a
>   neg :: a -> a
>   zero :: a

Mulable is a group, except that recp might fail.

recp is split into recpFast which always returns something
and recpSlow which performs a zero check

> class Mulable a where
>   mul :: a -> a -> a
>   recpFast :: a -> a
>   recpSlow :: a -> Maybe a
>   one :: a

Scalable captures the notion of allowing scalar multiplication

> class Scalable k a where
>   scale :: k -> a -> a

A "flat" rational interval consists of a lower and upper bound. The interval
should be thought of as *closed*. In other words, the interval (1001) below
contains both 4 and 5.

Note that it is possible to create an invalid literal if the lower bound is
greater than the upper bound

  FlatRat 4 5    (1001)

> data FlatRat = FlatRat {
>   low :: Rational,
>   high :: Rational
> } deriving (Eq)

Note that intervals are closed intervals. The alternative is possible but
renders many more operations inexact.

> instance UntraversableContainer Rational FlatRat where
>   contains k (FlatRat low high) = (low <= k) && (k <= high)

rational intervals can be added together without checking the bounds.

> instance Addable FlatRat where
>   add (FlatRat a b) (FlatRat c d) = (FlatRat (a + c) (b + d))
>   neg (FlatRat a b) = (FlatRat (negate b) (negate a))
>   zero = (FlatRat 0 0)

multiplication does require checking the bounds.

> instance Mulable FlatRat where
>   mul (FlatRat a b) (FlatRat c d) = out where
>     out = (FlatRat min' max')
>     xs = [a * c, a * d, b * c, b * d]
>     min' = minimum xs
>     max' = maximum xs
>
>   recpFast x@(FlatRat low high) =
>     if contains (0 :: Rational) x then (FlatRat 0 0)
>                                   else FlatRat (recip high) (recip low)
>   recpSlow x@(FlatRat low high) =
>     if contains (0 :: Rational) x then Nothing
>                                    else Just (FlatRat (recip high) (recip low))
>
>   one = FlatRat 1 1


scalar multiplication is straightforward

> instance Scalable Rational FlatRat where
>   scale k (FlatRat low high) | k >= 0 = (FlatRat (k * low) (k * high))
>                              | otherwise = (FlatRat (k * high) (k * low))


