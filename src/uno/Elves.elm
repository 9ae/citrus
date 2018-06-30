module Elves exposing (hl, tl, h2l, nl, popn, te)

import List exposing (take, drop, tail, head, reverse)

-- Get head as list
hl: List a -> List a
hl l = take 1 l

-- Get second element as list
h2l: List a -> List a
h2l l = drop 1 (take 2 l)

-- Get n-th element as list
nl: Int -> List a -> List a
nl n l = drop (n - 1) (take n l)

-- Get tail as list
tl: List a -> List a
tl l = drop 1 l

te: List a -> Maybe a
te l = head (reverse l)

popn: Int -> List a -> List a
popn n l = (take n l) ++ (drop (n + 1) l)
