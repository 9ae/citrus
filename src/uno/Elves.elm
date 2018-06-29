module Elves exposing (hl, tl)

import List exposing (take, drop)

hl: List a -> List a
hl l = take 1 l

tl: List a -> List a
tl l = drop 1 l
