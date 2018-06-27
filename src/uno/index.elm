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

init : (Model, Cmd Msg)
init =
  ((Model createDeck), Cmd.none)

-- Update

-- randomNDeck = Random.step fullDeckGenerator model.seed0

sliceDeck2: Array (Card) -> Int -> Int -> Array (Card) -> Array (Card)
sliceDeck2 cards i j topHalf = append (slice i j cards) (append topHalf (slice j 108 cards))

sliceDeck1: Array (Card) -> Int -> Array (Card)
sliceDeck1 cards i =  sliceDeck2 cards i (i//2) (slice 0 i cards)

shuffleIteration: Array (Card) -> Int -> Int -> Array (Card)
shuffleIteration cards it r =
  if it == 0 then cards else shuffleIteration (sliceDeck1 cards r) (it - 1) r

shuffle: List (Card) -> Int -> List (Card)
shuffle cards r = Array.toList (shuffleIteration (Array.fromList cards) 1 r)


type Msg = Start | Shuffle | Chaos Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Shuffle -> (model, Random.generate Chaos (Random.int 0 108))
    Chaos r -> ({ model | deck = shuffle model.deck r}, Cmd.none)
    _ -> (model, Cmd.none)


-- subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- View

drawCard : Card -> Html Msg
drawCard card = div [ style [("display", "inline-block"), ("width", "50px"), ("height", "50px"), ("text-align", "center"), ("margin", "2px"), ("border-width", "5px"),("border-style", "solid"), ("border-color", if card.color == "wild" then "black" else card.color)] ] [ text card.denom ]

view : Model -> Html Msg
view model = div [] [
    button [onClick Shuffle] [ text "Shuffle" ],
    div [] (List.map drawCard model.deck)
  ]


main =
  Html.program
    {
    init = init,
    view = view,
    update = update,
    subscriptions = subscriptions
    }
