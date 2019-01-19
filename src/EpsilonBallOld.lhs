I don't know which of these extensions we actually need

> module EpsilonBallOld where
> a = 7

< {-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
< {-# LANGUAGE AllowAmbiguousTypes #-}
< {-# LANGUAGE CPP #-}

< module EpsilonBallOld where

< import Data.Ratio (Rational, numerator, denominator)
< import qualified Data.Map as Map
< import Numeric.Natural (Natural)

First we need to define the types we need for expressing
what a rational interval is. We'll do this with a struct
of functions rather than a dedicated type class.

An EBInterval is the main interval type,

[add]ition and [mul]tiplication are addition and multiplication
of intervals. "scale" refers to multiplication by a scalar (it's a pun).

For an EBInterval specifically, taking the reciprocal of an interval
containing zero produces Nothing. That is the only condition under which
the multiplicative inverse does not exist.

k -- scalars                           (field of scalars is commonly abbreviated k)
i -- index                             (i for index)
a -- interval type                     (just a generic name)
m -- optional type representing errors (maybe)

< class EBInterval where
<   add :: a -> a

pretty print rational number, zero, negative numbers without parentheses.

< prettyPrintRational :: Rational -> String
< prettyPrintRational rat | (numerator rat == 0) =
<   "0"
< prettyPrintRational rat | (denominator rat == 1) =
<   show (numerator rat)
< prettyPrintRational rat =
<   show rat

A "flat" rational interval consists of a lower and upper bound. The interval
should be thought of as *closed*. In other words, the interval (1001) below
contains both 4 and 5.

Note that it is possible to create an invalid literal if the lower bound is
greater than the upper bound

  FlatRat 4 5    (1001)

< data FlatRat = FlatRat {
<   low :: Rational ,
<   high :: Rational
< } deriving (Eq)

print it like a Python tuple

< instance Show FlatRat where
<   show (FlatRat low high) =
<     ("FlatRat(" ++ prettyPrintRational  low ++ ", " ++
<                    prettyPrintRational high ++ ")")

flatRatEBI is the companion object defining the functions
on flatRats ... nothing too surprising

< instance EBInterval Rational FlatRat (Maybe FlatRat) where
<   contains k (FlatRat a b) = a <= k && k <= b
<   add (FlatRat a b) (FlatRat c d) =
<     FlatRat (a + c) (b + d)
<   mul(FlatRat a b) (FlatRat c d) =
<     let xs = [a * c, b * c, a * d, b * d] in
<     let low = minimum xs in
<     let high = maximum xs in
<     FlatRat low high
<   scale x (FlatRat a b) =
<     (if x >= 0 then FlatRat (x * a) (x * b)
<                else FlatRat (x * b) (x * a))
<   neg (FlatRat a b) = (FlatRat b a)
<   recp (FlatRat a b) =
<     (if ((a <= 0) && (0 <= b)) then Nothing
<                                else Just (FlatRat (recip b) (recip a)))

Smoke test. Make sure that the definitions above for FlatRat work
in at least one case

< -- | flatRat Smoke Test
< -- >>> add (FlatRat 1 2) (FlatRat 2 3)
< -- FlatRat(3, 5)
< -- >>> mul (FlatRat (-1) 1) (FlatRat (-1) 1)
< -- FlatRat(-1, 1)
< -- >>> scale (2/3) (FlatRat (-1) 1)
< -- FlatRat((-2) % 3, 2 % 3)
< -- >>> neg (FlatRat 1 2)
< -- FlatRat(-2, -1)
< -- >>> recp (FlatRat (-1) 1)
< -- Nothing
< -- >>> contains 0 (FlatRat (-1) 1)
< -- True
< -- >>> contains 24 (FlatRat (-1) 1)
< -- False


Next we need to define a sequence of intervals. There is an unenforced property
that the intervals must shrink.

< type RatIntervalSeq = Natural -> FlatRat

< instance EBInterval Rational RatIntervalSeq (Natural -> Maybe FlatRat) where
<   add f g i = add (f i) (g i)
<   mul f g i = mul (f i) (g i)
<   scale k x i = scale k (x i)
<   neg x i = neg (x i)
<   recp x i = recp (x i)
<   contains k x i = contains k (x i)

< data PseudoReal = PseudoReal {approx :: Rational -> Rational}

< newPseudoReal :: Rational -> PseudoReal
< newPseudoReal x = PseudoReal (const x)

< (+|) :: PseudoReal -> PseudoReal -> PseudoReal
< a +| b = PseudoReal (\x -> approx a (x/2) + approx b (x/2))

< scalarMul :: (Rational -> PseudoReal -> PseudoReal)
< scalarMul 0 _ = newPseudoReal $ fromInteger 0
< scalarMul c x = PseudoReal (\ eps -> c * approx x (eps/ abs c))

< (*|) :: PseudoReal -> PseudoReal -> PseudoReal
< a *| b = PseudoReal {approx = approxer} where
<  epses = [(1/2) ^ n | n <- [1 ..]]
<  pairs = [ approxMul i a b | i <- epses ]
<  approxer e = fst $ head $ dropWhile (\pair -> snd pair > e) pairs

< instance Num PseudoReal where
<  (+) = (+|)
<  (*) = (*|)
<  negate = scalarMul (-1)
<  signum x = undefined
<  abs x = undefined
<  fromInteger = newPseudoReal . fromInteger


< approxMul :: Rational -> PseudoReal -> PseudoReal -> (Rational, Rational)
< approxMul e x y =
<  let xe = approx x e in
<  let ye = approx y e in
<  ( xe * ye, e * (abs xe + abs ye + e) )

< data GenCF = GenCF {a :: Integer -> Integer, b :: Integer -> Integer}

< (//) :: Integer -> Integer -> Rational
< a // b = fromInteger a / fromInteger b


< root2 = GenCF {a = const 1, b = (\x -> if x == 0 then 1 else 2)}

< fib = GenCF {a = const 1, b = const 1}
