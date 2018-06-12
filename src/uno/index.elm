import Html exposing (Html, div, text, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Array exposing (Array, fromList, slice, append)

import Cards exposing (Card, createDeck)
import Random

fullDeckGenerator = Random.int 0 108

-- Model

type alias Model = {
  deck: List (Card)
}
model : Model
model = {
    deck = createDeck
  }

-- Update

-- randomNDeck = Random.step fullDeckGenerator model.seed0

sliceDeck2: Array (Card) -> Int -> Int -> Array (Card) -> Array (Card)
sliceDeck2 cards i j topHalf = append (slice i j cards) (append topHalf (slice j 108 cards))

sliceDeck1: Array (Card) -> Int -> Array (Card)
sliceDeck1 cards i =  sliceDeck2 cards i 80 (slice 0 i cards)

shuffleIteration: Array (Card) -> Int -> Array (Card)
shuffleIteration cards it =
  if it == 0 then cards else shuffleIteration (sliceDeck1 cards 36) (it - 1)

shuffle: List (Card) -> List (Card)
shuffle cards = Array.toList (shuffleIteration (Array.fromList cards) 1)


type Msg = Start | Shuffle | RGen (Int, Random.Seed)

update : Msg -> Model -> Model
update msg model =
  case msg of
    Shuffle -> { model | deck = shuffle model.deck }
    _ -> model

-- View

drawCard : Card -> Html Msg
drawCard card = div [ style [("display", "inline-block"), ("width", "50px"), ("height", "50px"), ("text-align", "center"), ("margin", "2px"), ("border-width", "5px"),("border-style", "solid"), ("border-color", if card.color == "wild" then "black" else card.color)] ] [ text card.denom ]

view : Model -> Html Msg
view model = div [] [
    button [onClick Shuffle] [ text "Shuffle" ],
    div [] (List.map drawCard model.deck)
  ]


main =
  Html.beginnerProgram
    { model = model
    , view = view,
    update = update
    }
