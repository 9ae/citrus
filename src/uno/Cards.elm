module Cards exposing (Card, createDeck)

type alias Card = {
    denom: String,
    color: String
}

createCard : String -> String -> Card
createCard d c = { denom = d, color = c }

createPerColor : String -> List (Card)
createPerColor d = List.map (\c -> createCard d c) ["red", "blue", "yellow", "green"]

createNCardsPerColor : String -> Int -> List (Card)
createNCardsPerColor d n = List.concatMap (\c -> List.repeat n (createCard d c)) ["red", "blue", "yellow", "green"]

createNumberCards: List (Card)
createNumberCards = List.concatMap (\d -> createNCardsPerColor d 2) (List.map (\x -> toString x) (List.range 1 9))

createActionCards: List (Card)
createActionCards = List.concatMap (\d -> createNCardsPerColor d 2) ["skip", "rev", "draw2"]

createWildCards: List (Card)
createWildCards = (List.repeat 4 (createCard "wild" "wild")) ++ (List.repeat 4 (createCard "draw4" "wild"))

createDeck: List (Card)
createDeck = (createPerColor "0") ++ (createNumberCards) ++ (createActionCards) ++ (createWildCards)
