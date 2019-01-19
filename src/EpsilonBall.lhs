> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE TypeSynonymInstances #-}

> module EpsilonBall where

> import Data.Ratio (Rational, numerator, denominator)
> import qualified Data.Map as Map
> import Numeric.Natural (Natural)

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

Pretty print a rational number. The pretty-printer does not detect malformed
rational numbers. I don't know what kind of malformed-ness is ruled out by the
Rational API.

> prettyPrintRat :: Rational -> String
> prettyPrintRat x = out where
>   num = numerator x
>   denom = denominator x
>   f a b | a == 0 = "0"
>         | b == 1 = show a
>         | b == -1 = show (negate a)
>         | otherwise = show x
>   out = f num denom

Check that a rational number is pretty printed correctly.

> -- | Pretty Print Rat
> -- >>> prettyPrintRat 0
> -- "0"
> -- >>> prettyPrintRat 1
> -- "1"
> -- >>> prettyPrintRat (-1)
> -- "-1"
> -- >>> prettyPrintRat (1/2)
> -- "1 % 2"
> -- >>> prettyPrintRat ((-1)/2)
> -- "(-1) % 2"

Print an individual FlatRat in the approximate style of an instance of a Python class.
It is okay to for this implementation of Show FlatRat to be in the Show typeclass
rather than being a dedicated pretty-print function because the representation is lossless.

> instance Show FlatRat where
>   show (FlatRat low high) = "FlatRat(" ++ show low ++ ", " ++ show high ++ ")"

> -- | Show FlatRat
> -- >>> FlatRat (1/2) (3/4)
> -- FlatRat(1 % 2, 3 % 4)

A RatIntervalSeq is a sequence of rational intervals.
It cannot be directly compared for equality, but it can be added, multiplied and scaled.
Scalar multiplication is constant time.

Note, we can't actually identify rational interval sequences that
are guaranteed to be finite.

Consider restricting rational interval sequences so that they are either
strictly narrowing or constant.

A RatIntervalSeq is opaque, except for the scalar attached to it.

> data RatIntervalSeq = RatIntervalSeq {
>   rISScalar :: Rational,
>   riSFunc   :: Natural -> FlatRat
> }

Addition related functions.
Addition itself always produces a new normalized entity with 1 as the scalar.
Negation is implemented by negating the scalar.

> instance Addable RatIntervalSeq where
>   add (RatIntervalSeq sc f) (RatIntervalSeq sc' f') =
>     (RatIntervalSeq 1 (\ n -> (add (scale sc  (f  n))
>                                    (scale sc' (f' n)))))
>   neg (RatIntervalSeq sc f) = (RatIntervalSeq (negate sc) f)
>   zero = (RatIntervalSeq 0 (const zero))

multiplication. recpFast and recpSlow not defined yet

> instance Mulable RatIntervalSeq where
>   mul (RatIntervalSeq sc f) (RatIntervalSeq sc' f') =
>     (RatIntervalSeq (sc * sc') (\n -> mul (f n) (f' n)))
>   recpFast = undefined
>   recpSlow = undefined
>   one = (RatIntervalSeq 1 (const one))

scalar multiplication.

> instance Scalable Rational RatIntervalSeq where
>   scale k (RatIntervalSeq sc f) = (RatIntervalSeq (k * sc) f)
