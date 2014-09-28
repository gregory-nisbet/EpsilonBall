import Data.Ratio (Rational)
import qualified Data.Map as Map

data GenCF = GenCF {a :: Integer -> Integer, b :: Integer -> Integer}

data Convergents = Convergents {
  values :: [Rational],
  aList :: [Integer],
  bList :: [Integer],
  aa :: [Integer],
  bb :: [Integer]
}

initial :: GenCF -> Convergents
initial g = Convergents {
  values = [], aa = [], bb = [],
  aList = map (a g) [0 ..], bList = map (b g) [0 ..]
}

nth :: [a] -> Integer -> a
nth lst n = lst !! (fromInteger n)

(//) :: Integer -> Integer -> Rational
x // y = fromInteger x / fromInteger y

enhance :: Convergents -> Convergents
enhance c =
  c {aa = aas, bb = bbs, values = values'} where
    a = aList c
    b = bList c
    aas = map aa' [0 ..]
    bbs = map bb' [0 ..]

    aa' :: Integer -> Integer
    aa' (-1) = 0
    aa' 0 = nth b 0
    aa' 1 = (nth b 1) * (nth b 0) + (nth a 1)
    aa' n 
      | n < 0 = undefined
      | otherwise = 
        nth b n * nth aas (n-1) + nth a n * nth aas (n-2)  

    bb' :: Integer -> Integer
    bb' (-1) = 0
    bb' 0 = 1
    bb' 1 = (nth b 1)
    bb' n
      | n < 0 = undefined
      | otherwise =
        nth b n * nth bbs (n-1) + nth a n * nth bbs (n-2)

    values' :: [Rational]
    values' = map (\i -> aa' i // bb' i) [0 ..]

root2 = enhance $ initial GenCF {a = const 1, b = (\x -> if x == 0 then 1 else 2)}

piCF = enhance $ initial GenCF {
  a = \n -> if n == 1 then 4 else (n-1)^2,
  b = \n -> if n == 0 then 0 else 2*n -1
}

snippet :: Convergents -> [Double]
snippet c = map fromRational $ take 20 $ values c
