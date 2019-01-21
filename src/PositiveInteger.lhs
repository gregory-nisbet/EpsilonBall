A type for representing Natural Numbers starting from one, aka positive
integers. It is a seperate module for encapsulation purposes.
The implementation of this module happens to wrap an existing implementation of
natural numbers including zero, but that isn't guaranteed.

> module PositiveInteger
> (PositiveInteger, fromNatural) where
> import Numeric.Natural (Natural)
> import Data.Maybe (fromJust)

Use a data instead of a newtype so we have more control.
The default implementations of Eq and Ord are a good match for
what we're doing.

> data PositiveInteger = PositiveInteger Natural deriving (Eq, Ord)

A PositiveInteger prints the same as its data member.

> instance Show PositiveInteger where
>   show (PositiveInteger x) = show x

Check that printing shows just the inner number.

> -- | Show PositiveInteger
> -- >>> show (PositiveInteger 7)
> -- "7"

dedicated function for converting from a natural number
to a positive integer option. The conversion fails
if and only if the given natural number was zero.

> fromNatural :: Natural -> Maybe PositiveInteger
> fromNatural x = if x == 0 then Nothing else Just $ PositiveInteger x

And we test it.

> -- | fromNatural
> -- >>> fromNatural 0
> -- Nothing
> -- >>> fromNatural 1
> -- Just 1

For convenience, we implement the Num typeclass, even though it is
not an exact fit for this type.
In particular, attempting to create a positive int out of 0 or
a negative integer literal fails.

Addition always succeeds.
Multiplication always succeeds.
Negation always fails.
taking the absolute value always succeeds.
taking the signum always succeeds.

fromInteger succeeds if and only if the number is positive.

> instance Num PositiveInteger where
>   (PositiveInteger x) + (PositiveInteger y) = PositiveInteger (x + y)
>   (PositiveInteger x) * (PositiveInteger y) = PositiveInteger (x * y)
>   abs x = x
>   signum x = 1
>   fromInteger x = fromJust $ fromNatural $ fromInteger x
>   negate x = undefined

basic test of Num functionality.

> -- | fromNatural num
> -- >>> 1 :: PositiveInteger
> -- 1
> -- >>> (1 + 2) :: PositiveInteger
> -- 3
> -- >>> (7 * 2) :: PositiveInteger
> -- 14
