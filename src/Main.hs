module Main where

import qualified Data.Set as Set 
import Data.Set (Set)
import qualified Data.List as List
import Data.Numbers.Primes 
import System.Environment (getArgs)
import Data.Ratio

main :: IO ()
main = do
  args <- getArgs
  let (firstArg: rest) = args 
  if firstArg == "-h" || firstArg == "--help" 
     then print "args: level dice1 dice2 ..."
     else do
       let level = read firstArg :: Int 
       let dice = map (%1) $ map read rest:: [Ratio Integer]
       print $ achieves level dice
       print $ case (achieves2 level dice) of 
                 Nothing -> ([], 0)
                 Just a -> (map fromRational (fst a), fromRational (uncurry apply a))

ops :: Fractional a => [a -> a -> a]
ops = [(+), (-), (*), (/)]

slice :: Int -> Int -> [a] -> [a]
slice a b xs = (take (b - a)) . (drop a) $ xs

targetPrimes :: Fractional a => Int -> [a]
targetPrimes level = map fromIntegral (slice start (start + 3) (drop 1 primes))
  where start = (level - 1) * 3

achieves :: (Fractional a, Ord a) => Int -> [a] -> Bool
achieves level numbers = or $ hasTarget targets results
  where numbersPermutated = List.permutations numbers
        opsPermutated :: Fractional a => [[a -> a -> a]]
        opsPermutated = fillSlots ops ((length numbers) - 1) 
        targets :: (Fractional a,  Ord a) => Set a
        targets = Set.fromList . targetPrimes $ level
        dual = dualPermu numbersPermutated opsPermutated
        results = map (uncurry apply) $ dual

hasTarget :: Ord a => Set a -> [a] -> [Bool]
hasTarget targets = map (flip Set.member $ targets)

achieves2 :: (Fractional a, Ord a) => Int -> [a] -> Maybe ([a], [a -> a -> a])
achieves2 level numbers = getOps xs dual
  where numbersPermutated = List.permutations numbers
        opsPermutated :: Fractional a => [[a -> a -> a]]
        opsPermutated = fillSlots ops ((length numbers) - 1) 
        targets :: (Fractional a,  Ord a) => Set a
        targets = Set.fromList . targetPrimes $ level
        dual = dualPermu numbersPermutated opsPermutated
        results = map (uncurry apply) $ dual
        xs = hasTarget targets results 

getOps [] [] = Nothing
getOps (x:xs) (y:ys) = if x then Just y else getOps xs ys

dualPermu :: [a] -> [b] -> [(a, b)]
dualPermu xs ys = concatMap f ys
  where 
        f y = map (\x -> (x, y)) xs 

fillSlots :: [a] -> Int -> [[a]]
fillSlots elements 0 = []
fillSlots elements 1 = map (: []) elements
fillSlots elements slots = concatMap (add1 elements) (fillSlots elements (slots - 1))

add1 :: [a] -> [a] -> [[a]]
add1 elements xs = map (: xs) elements

apply :: Fractional a => [a] -> [a -> a -> a] -> a
apply (num:[]) [] = num
apply (num1:num2:xs) (op:ys) = apply ((op num1 num2) : xs) ys
