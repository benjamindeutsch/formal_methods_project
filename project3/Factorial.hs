module Factorial where

{-@
factorial :: n:Nat -> {i:Int | i > 0 && i>=n} / [n]
@-}
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

{-@
rising_factorial :: n:Nat -> k:{j: Int | j>=0 && j<=n} -> {i:Int | i > 0 && i>=k} / [k]
@-}
rising_factorial :: Int -> Int -> Int
rising_factorial n 0 = 1
rising_factorial n k = n * rising_factorial (n + 1) (k-1)

{-@
falling_factorial :: n:Nat -> k:{j: Int | j>=0 && j<=n} -> {i:Int | i > 0 && i>=k} / [k]
@-}
falling_factorial :: Int -> Int -> Int
falling_factorial n 0 = 1
falling_factorial n k = n * falling_factorial (n - 1) (k-1)