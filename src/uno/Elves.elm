module Elves exposing (hl, tl, h2l)

import List exposing (take, drop)

-- Get head as list
hl: List a -> List a
hl l = take 1 l

-- Get second element as list
h2l: List a -> List a
h2l l = drop 1 (take 2 l)

-- Get tail as list
tl: List a -> List a
tl l = drop 1 l
