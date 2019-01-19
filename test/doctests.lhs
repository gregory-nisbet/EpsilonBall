Doctests driver taken from
https://www.reddit.com/r/haskell/comments/3zqv2e/using_doctestdiscover_with_stack/cyoaorg

> module Main (main) where

> import System.FilePath.Glob (glob)
> import Test.DocTest (doctest)
> import Text.Printf (printf)

Note, using relatives paths to move us back to
the project root is really inelegant.

> hs :: [Char]
> hs = "./src/*.hs"
> lhs :: [Char]
> lhs = "./src/*.lhs"

> showHs :: IO ()
> showHs = (do
>   xs <- glob hs
>   printf "\n%s" (show xs))

> showLhs :: IO ()
> showLhs = (do
>   xs <- glob lhs
>   printf "\n%s" (show xs))

> main :: IO ()
> main =
>   showHs >>
>   showLhs >>
>   printf "\n" >>
>   (do
>     glob hs  >>= doctest
>     glob lhs >>= doctest)

< import Test.DocTest
< main = doctest ["./src/EpsilonBall.hs"]
