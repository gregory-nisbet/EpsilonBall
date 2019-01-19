module EpsilonBall where

import Data.Ratio (Rational)
import qualified Data.Map as Map

-- |
-- >>> putStrLn "b"
-- b

data PseudoReal = PseudoReal {approx :: Rational -> Rational}

newPseudoReal :: Rational -> PseudoReal
newPseudoReal x = PseudoReal (const x)

(+|) :: PseudoReal -> PseudoReal -> PseudoReal
a +| b = PseudoReal (\x -> approx a (x/2) + approx b (x/2))

scalarMul :: (Rational -> PseudoReal -> PseudoReal)
scalarMul 0 _ = newPseudoReal $ fromInteger 0
scalarMul c x = PseudoReal (\ eps -> c * approx x (eps/ abs c))

(*|) :: PseudoReal -> PseudoReal -> PseudoReal
a *| b = PseudoReal {approx = approxer} where
 epses = [(1/2) ^ n | n <- [1 ..]]
 pairs = [ approxMul i a b | i <- epses ]
 approxer e = fst $ head $ dropWhile (\pair -> snd pair > e) pairs

instance Num PseudoReal where
 (+) = (+|)
 (*) = (*|)
 negate = scalarMul (-1)
 signum x = undefined
 abs x = undefined
 fromInteger = newPseudoReal . fromInteger


approxMul :: Rational -> PseudoReal -> PseudoReal -> (Rational, Rational)
approxMul e x y =
 let xe = approx x e in
 let ye = approx y e in
 ( xe * ye, e * (abs xe + abs ye + e) )

data GenCF = GenCF {a :: Integer -> Integer, b :: Integer -> Integer}

(//) :: Integer -> Integer -> Rational
a // b = fromInteger a / fromInteger b


root2 = GenCF {a = const 1, b = (\x -> if x == 0 then 1 else 2)}

fib = GenCF {a = const 1, b = const 1}
