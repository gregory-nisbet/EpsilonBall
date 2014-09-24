import Data.Ratio (Rational)

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

approxMul :: Rational -> PseudoReal -> PseudoReal -> (Rational, Rational)
approxMul e x y = 
  let xe = approx x e in
  let ye = approx y e in
  ( xe * ye, e * (abs xe + abs ye + e) )

data GenCF = GenCF {a :: Integer -> Integer, b :: Integer -> Integer}

(//) :: Integer -> Integer -> Rational
a // b = fromInteger a / fromInteger b

generalizedCF :: GenCF -> Integer -> Rational
generalizedCF gen i 
  | i < 0 = undefined
  | otherwise= aa i // bb i where
    aa (-1) = 1
    aa 0 = (b gen 0)
    aa num = (b gen (n+1)) * aa n + (a gen (n+1)) * aa (n-1) where 
      n = num - 1

    bb (-1) = 0
    bb 0 = 1
    bb num = (b gen (n+1)) * bb n + (a gen (n+1)) * bb (n-1) where
      n = num - 1

root2 = GenCF {a = const 1, b = (\x -> if x == 0 then 1 else 2)}

fib = GenCF {a = const 1, b = const 1}



