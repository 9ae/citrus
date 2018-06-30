module Cards exposing (
    Card,
    createDeck,
    divideDeck,
    filterActionCards,
    getFirstNumeric,
    moveNCards,
    validateAdjacentCards
  )

import List exposing (length, take, drop, isEmpty, repeat, concatMap, map, repeat, range, filter, filterMap, head, tail)
import Tuple exposing (first, second)
import Elves exposing (hl, h2l, tl)
-- import Maybe exposing (Maybe)

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

isNumberic: Maybe Card -> Bool
isNumberic m =
  case m of
    Just card -> Result.toMaybe (String.toInt card.denom) /= Nothing
    Nothing -> False

isAction: Maybe Card -> Bool
isAction m = not (isNumberic m)

deckTailAppend: List (Card) -> Maybe Card -> List (Card)
deckTailAppend deck card =
  case card of
    Just card -> (tl deck) ++ [card]
    Nothing -> tl deck

findFirstNumeric: (Maybe Card, List (Card)) -> (Maybe Card, List (Card))
findFirstNumeric deck =
  if isNumberic (first deck) then deck
  else findFirstNumeric ((head (second deck)), (deckTailAppend (second deck) (first deck)))

getFirstNumeric: List (Card) -> (Maybe Card, List(Card))
getFirstNumeric deck = findFirstNumeric ((head deck), (tl deck))

validateColors: Card -> Card -> Bool
validateColors prevCard nextCard =
  if nextCard.color == "wild" || prevCard.color == "wild" then True
  else prevCard.color == nextCard.color

validateDemom: Card -> Card -> Bool
validateDemom prevCard nextCard = prevCard.denom == nextCard.denom

validateAdjacentCards: Maybe Card -> Card -> Bool
validateAdjacentCards prevCard nextCard =
  case prevCard of
    Just card -> (validateColors card nextCard) || (validateDemom card nextCard)
    Nothing -> False

moveNCards: Int -> List (Card) -> List (Card) -> List (Card)
moveNCards n deck hand = hand ++ (take n deck)

filterActionCards: List (Card) -> List (Card)
filterActionCards cards = filter (\c -> not (isNumberic (Just c))) cards
