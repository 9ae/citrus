module Cards exposing (Card, createDeck, divideDeck)

import List exposing (length, take, drop, isEmpty, repeat, concatMap, map, repeat, range)
import Tuple exposing (first, second)
import Elves exposing (hl, h2l, tl)

type alias Card = {
    denom: String,
    color: String
}

-- Create Deck

createPerColor : String -> List (Card)
createPerColor d = map (\c -> Card d c) ["red", "blue", "yellow", "green"]

createNCardsPerColor : String -> Int -> List (Card)
createNCardsPerColor d n = concatMap (\c -> repeat n (Card d c)) ["red", "blue", "yellow", "green"]

createNumberCards: List (Card)
createNumberCards = concatMap (\d -> createNCardsPerColor d 2) (map (\x -> toString x) (range 1 9))

createActionCards: List (Card)
createActionCards = concatMap (\d -> createNCardsPerColor d 2) ["skip", "rev", "draw2"]

createWildCards: List (Card)
createWildCards = (repeat 4 (Card "wild" "wild")) ++ (repeat 4 (Card "draw4" "wild"))

createDeck: List (Card)
createDeck = (createPerColor "0") ++ (createNumberCards) ++ (createActionCards) ++ (createWildCards)

-- Other deck fucntions

divideDeck: List (Card) -> (List (Card), List (Card)) -> (List (Card), List (Card))
divideDeck cards results =
  if (length cards) < 2 then results
  else divideDeck (drop 2 cards) ((first results) ++ (hl cards), (second results) ++ (h2l cards))
