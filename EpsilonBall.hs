import Data.Ratio (Rational)

newtype Real = Real {approx :: Rational -> Rational}

fromRational :: Rational -> Real
fromRational x = Real (const x)

(+|) :: Real -> Real -> Real
a +| b = Real (\x -> approx a (x/2) + approx b (x/2))

scalarMul :: (Rational -> Real -> Real) 
scalar 0 _ = fromRational $ fromInteger 0
scalar c x = Real (\ eps -> c * approx x (eps/ abs c))

(*|) :: Real -> Real -> Real
a *| b = Real (approx = approxer) where
  epses = [(1/2) ^ n | n <- [1 ..]]
  pairs = [ approxMul i a b | i <- epses ]
  approxer e = fst $ head $ dropWhile (\pair -> snd pair> e) pairs

approxMul :: Rational -> Real -> Real -> (Rational, Rational)
approxMul e x y = 
  let xe = approx x e in
  let ye = approx y e in
  ( xe * ye, e * (abs xe + abs ye + e) )

