-- Haskell Laziness Lab
-- CS 330
-- Braden Hitchcock
-- 4.14.2017
--

-- SETTING THINGS UP...
-- Import the array library for use with Part 2 (LCS)
import Data.Array
--
-- Helper Functions for Part 1 and 2
--
-- iSqrt
-- Gives the integer representation of the square root of a number
iSqrt :: Int-> Int
iSqrt n = floor(sqrt(fromIntegral n))
-- longest
-- returns the longest of two strings
longestStr :: String -> String -> String
longestStr xs ys = if length xs > length ys then xs else ys
-- getStrLcs
-- gives the string representation of the longest common subsequence
-- between two strings
getStrLcs :: String -> String -> String
getStrLcs xs ys = table!(0,0) where
  -- get the lengths
  lengthx = length xs
  lengthy = length ys
  -- initilize the 'array' (really a list, table in the algorithm)
  table = array ((0,0),(lengthx,lengthy)) $
    -- set up the first row (all empty strings)
    [((i,lengthy),[]) | i <- [0..lengthx]] ++
    -- set up the first column (all empty strings)
    [((lengthx,j),[]) | j <- [0..lengthy]] ++
    -- match each character in the strings with an index for access
    -- and then pass values into a function to build the result at
    -- each table cell
    [((i,j), buildPartialResult x y i j) | (x,i) <- zip xs [0..], (y,j) <- zip ys [0..]]
  -- Inner function to build result
  buildPartialResult x y i j 
    -- if the two characters match, add it to the result string (set the pointer)
    | x == y    = x : table!(i+1,j+1)
    -- if the two characters don't match, look into the next longest
    -- possible sequence based on neighboring characters
    -- while working up the table and set the cell pointer to that character
    | otherwise = longestStr (table!(i,j+1)) (table!(i+1,j))



-- LET'S GET STARTED!
--
-- Part 1: Primality (Normal and Lazy Evaluations)
--
-- isPrime
-- Given a number, returns true if it is prime, false otherwise
isPrime :: Int -> Bool
isPrime x = null [n | n <- [2..(iSqrt x)], (mod x n) == 0]
-- prime
-- An infinite list of all primes
primes :: [Int]
primes = filter isPrime [2..]
-- isPrimeFast
-- Takes an integer x and returns true if it is prime
-- Only tests prime factors from the primesFast list
isPrimeFast :: Int -> Bool
isPrimeFast x = null $ filter (\y -> ((mod x y) == 0)) $ takeWhile(<=(iSqrt x)) primesFast
-- primesFast
-- Infinite loops of all primes and names it primesFast (constructed by isPrimesFast)
primesFast :: [Int]
primesFast = 2 : filter isPrimeFast [3,5..]


--
-- Part 2: Longest Common Subsequence
--
-- lcsLength
-- Computes the length of the longest common subsequence of two strings
-- (Note: strings in haskell are lists of characters)
-- Uses the helper function 'getStrLcs' defined above in the helper function section
lcsLength :: String -> String -> Int
lcsLength xs ys = length (getStrLcs xs ys)






