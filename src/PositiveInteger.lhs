A type for representing Natural Numbers starting from one, aka positive
integers. It is a seperate module for encapsulation purposes.
The implementation of this module wraps an existing implementation of
natural numbers including zero, but that isn't guaranteed.

> module PositiveInteger (PositiveInteger) where
> import Numeric.Natural (Natural)

> newtype PositiveInteger = PositiveInteger Natural
