Design issues:

Division is by far the hardest thing to get right conceptually.

It is worth splitting division out from the multiplication class
since division works differently for intervals and telescoping sequences of intervals.

Another thorny issue is monotonicity.

I'd like to devise a system in which every sequence of intervals is either
a) constant
b) monotonically shrinking

and I would like to define operations on intervals (e.g. sin, cos, tan) in such a
way that the constant-or-monotonically-shrinking property is preserved.

Another design issue is printing things.

The rule that is that types that implement Eq must have an invertible show function.
Types that do not implement Eq need not have an invertible show function.

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

> class Mulable a where
>   mul :: a -> a -> a
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
>   show (FlatRat low high) = "FlatRat(" ++ prettyPrintRat low ++ ", " ++ prettyPrintRat high ++ ")"

> -- | Show FlatRat
> -- >>> FlatRat (1/2) (3/4)
> -- FlatRat(1 % 2, 3 % 4)

In certain circumstances, we want a very compact representation of a
FlatRat that mirrors math notation, i.e. [x, y] for the interval FlatRat(x, y).
A degenerate interval is printed as just the number itself

> prettyPrintFlatRat :: FlatRat -> String
> prettyPrintFlatRat (FlatRat a b) =
>   if a == b then lower else bracketed where
>     bracketed = "[" ++ prettyPrintRat a ++ ", " ++ prettyPrintRat b ++ "]"
>     lower     = prettyPrintRat a

> -- | prettyPrintFlatRat
> -- >>> putStrLn $ prettyPrintFlatRat $ FlatRat (1/2) (1/2)
> -- 1 % 2
> -- >>> putStrLn $ prettyPrintFlatRat $ FlatRat (-9) 13
> -- [-9, 13]

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

multiplication.

> instance Mulable RatIntervalSeq where
>   mul (RatIntervalSeq sc f) (RatIntervalSeq sc' f') =
>     (RatIntervalSeq (sc * sc') (\n -> mul (f n) (f' n)))
>   one = (RatIntervalSeq 1 (const one))

scalar multiplication.

> instance Scalable Rational RatIntervalSeq where
>   scale k (RatIntervalSeq sc f) = (RatIntervalSeq (k * sc) f)

When show-ing a RatIntervalSeq, we arbitrarily pick the first three terms
and then show an ellipsis afterwards, under the assumption that the first three terms
are relatively cheap to compute.

This representation is even lossier than it has to be
because the underlying scalar multiple is obscured.

A lossy printed representation is okay for this type because
a) it includes an explicit ellipsis in the results, so a human would not be confused.
b) a RatIntervalSeq very intentionally does not implement Eq

Since the representation is lossy anyway, it is probably worth investigating whether
to print the numbers in decimal or not.

> instance Show RatIntervalSeq where
>   show (RatIntervalSeq sc f) =
>     out where
>       out = "RIS(" ++ first ++ ", " ++ second ++ ", " ++ third ++ ", " ++ "..." ++ ")"
>       first = prettyPrintFlatRat $ scale sc (f 1)
>       second = prettyPrintFlatRat $ scale sc (f 2)
>       third = prettyPrintFlatRat $ scale sc (f 3)

Extremely simple test with the [0, 0] interval. This is just here to spot check some of
the moving parts.

> -- | Show RatIntervalSeq
> -- >>> (zero :: RatIntervalSeq)
> -- RIS(0, 0, 0, ...)

Generalized Continued Fractions are an important source of narrowing interval sequences.

> data GenCF = GenCF {a :: Natural -> Integer, b :: Natural -> Integer}
