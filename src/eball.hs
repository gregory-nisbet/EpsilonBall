module Eball where

import Numeric.Natural
import Control.Applicative

{- The purpose of this module is to represent Approximate values.
 - (Although perhaps the term *sequence* might be better).
 - Really, it is no different than a simple function where the input
 - type is a natural number, except that known-constant 
 - functions get their own constructor, and numerical operations are defined
 - if the underlying type is Num.
 -
 - This is hopefully the first step towards a more complete interval
 - arithmetic implementaiton.
 -
 - The motivation here is to capture the semantics of things like
 - (generalized) continued fractions and Taylor / MacLaurin series that
 - converge to a particular value. 
 -
 - Continued fractions have the nice property that successive convergents
 - are alternating over and under-approximations and that any given convergent
 - is a best rational approximation in the sense that there is no rational number
 - with a smaller denominator that is closer to the actual number.
 -
 - MacLaurin series are convenient for defining things, but aren't guaranteed
 - to converge at all, and I can't seem to find how to get *absolute* error bounds
 - for the nth partial sum rather than approximate error bounds or an O(whatever) 
 - remainder term that bounds the complexity of the real error function.
 -
 - This is the first stab at a library for allowing users to work with these things
 - coherently. 
 -
 -}

-- we either have an exact number
-- or a way of getting an arbitrary precise number
-- by allocating a certain amount of "resources"
data Approximate a = Exact a | Approximate (Natural -> a)

-- if it is an exact number, we ignore the resource number
-- and just return the exact number
eval :: Approximate a -> Natural -> a
eval (Approximate generator) = generator
eval (Exact x) = const x

-- flatten will take nested approximate values
-- and collapse them by applying the name resource number to the inner and outer
-- if need be.
-- if the outermost value is exact we eliminate it
flatten :: Approximate (Approximate a) -> Approximate a
flatten (Exact (Exact x)) = Exact x
flatten (Exact (Approximate f)) = Approximate f
flatten (Approximate generator) =
    let newGenerator resource = eval (generator resource) resource in
    Approximate newGenerator

-- An Approximate of a Numeric is Numeric
-- the resource number is distributed to subexpressions
instance Num a => Num (Approximate a) where
    Exact a + Exact b = Exact (a + b)
    Exact a + Approximate generator = Approximate (\s -> a + generator s)
    Approximate generator + Exact b = Approximate (\s -> generator s + b)
    Approximate g + Approximate h = Approximate (\s -> g s + h s)

    negate (Exact a) = Exact (negate a)
    negate (Approximate fn) = Approximate (\s -> negate $ fn s)

    Exact a * Exact b = Exact (a + b)
    Exact a * Approximate generator = Approximate (\s -> a * generator s)
    Approximate generator * Exact b = Approximate (\s -> generator s * b)
    Approximate g * Approximate h = Approximate (\s -> g s * h s)

    fromInteger int = Exact $ fromInteger int

    abs (Exact a) = Exact (abs a)
    abs (Approximate fn) = Approximate (\s -> abs $ fn s)

    signum (Exact a) = Exact (signum a)
    signum (Approximate fn) = Approximate (\s -> signum $ fn s)

-- an Approximate is a functor in the obvious way.
instance Functor Approximate where
    fmap fun (Exact a) = Exact (fun a)
    fmap fun (Approximate fn) = Approximate (\s -> fun $ fn s)

-- an Approximate is applicative as well
-- we distribute the resource number over the Approximate function and the Approximate argument
instance Applicative Approximate where
    pure = Exact

    (Exact fun) <*> arg = fmap fun arg
    (Approximate approxFun) <*> arg = flatten $ Approximate (\s -> fmap (approxFun s) arg)

-- An Approximate is a monad
-- given an approximate value and a function requiring a raw value,
-- we can apply the resource number to the first approximate value and then apply the 
instance Monad Approximate where 
    return x = Exact x

    (Exact x) >>= f = f x
    (Approximate generator) >>= f = flatten $ Approximate (\s -> f (generator s))
