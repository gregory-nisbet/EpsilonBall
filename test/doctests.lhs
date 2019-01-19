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
>     xs1 <- glob hs
>     xs2 <- glob lhs
>     doctest (xs1 ++ xs2))

< import Test.DocTest
< main = doctest ["./src/EpsilonBall.hs"]
