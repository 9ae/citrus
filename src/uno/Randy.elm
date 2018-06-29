module Randy exposing (shuffle)

import List exposing (length, take, drop, isEmpty)
import Tuple exposing (first, second)
import Random exposing (Seed, int, step)

import Elves exposing (hl, tl)

shake: Seed -> Int -> Int -> (Int, Seed)
shake seed i j = step (int i j) seed

shakeInList: List a -> Seed -> (Int, Seed)
shakeInList l seed = shake seed 1 (length l - 2)

shuffle3: List a -> List a -> List a -> List a
shuffle3 a b result =
  if isEmpty a then
    result ++ b
  else if isEmpty b then
    result ++ a
  else
    shuffle3 (tl a) (tl b) (result ++ (hl a) ++ (hl b))

shuffle2: List a -> Int -> List a
shuffle2 cards r = shuffle3 (take r cards) (drop r cards) []

shuffle1: List a -> Int -> (Int, Seed) -> List a
shuffle1 cards it rs = if it == 0 then cards else
  shuffle1 (shuffle2 cards (first rs)) (it - 1) (shakeInList cards (second rs))

shuffle: List a-> Random.Seed -> List a
shuffle cards seed = shuffle1 cards 50 (shakeInList cards seed)
