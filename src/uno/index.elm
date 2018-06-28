import Html exposing (Html, div, text, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Array exposing (Array, fromList, slice, append, isEmpty, get, length)

import Cards exposing (Card, createDeck)
import Random

-- Model

type alias Model = {
  deck: List (Card),
  seed: Random.Seed
}

init : (Model, Cmd Msg)
init =
  ((Model createDeck (Random.initialSeed 1982211)), Cmd.none)

-- Update

-- randomNDeck = Random.step fullDeckGenerator model.seed0

shake: Random.Seed -> Int -> Int -> (Int, Random.Seed)
shake seed i j = Random.step (Random.int i j) seed

-- sliceDeck2: Array (Card) -> Int -> Int -> Array (Card) -> Array (Card)
-- sliceDeck2 cards i j topHalf = append (slice i j cards) (append topHalf (slice j 108 cards))
--
-- sliceDeck1: Array (Card) -> Int -> Array (Card)
-- sliceDeck1 cards i =  sliceDeck2 cards i (i//2) (slice 0 i cards)
--
-- shuffleIteration: Array (Card) -> Int -> Int -> Array (Card)
-- shuffleIteration cards it seed =
--   if it == 0 then cards else shuffleIteration (sliceDeck1 cards (shake 0 108 seed).first) (it - 1) (shake 0 108 seed).second

arrayHead: Array (Card) -> Array (Card)
arrayHead a = slice 0 1 a

arrayTail: Array (Card) -> Array (Card)
arrayTail a = (slice 1 (length a) a)

recombineDeck: Array (Card) -> Array (Card) -> Array (Card) -> Array (Card)
recombineDeck a b result =
  if isEmpty a then
    append result b
  else if isEmpty b then
    append result a
  else
    recombineDeck (arrayTail a) (arrayTail b) (append result (append (arrayHead a) (arrayHead b)))

newDeck: Array (Card) -> Int -> Array (Card)
newDeck cards r = recombineDeck (slice r 108 cards) (slice 0 r cards) (Array.empty)

cutDeck: Array (Card) -> Int -> (Int, Random.Seed) -> Array (Card)
cutDeck cards it rs =
  case it of
    0 -> cards
    _ -> cutDeck (newDeck cards (Tuple.first rs)) (it - 1) (shake (Tuple.second rs) 0 107)

shuffle: List (Card) -> Random.Seed -> List (Card)
shuffle cards seed = Array.toList (cutDeck (Array.fromList cards) 500 (shake seed 0 107))


type Msg = Start | Shuffle

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Shuffle -> ({ model | deck = shuffle model.deck model.seed}, Cmd.none)
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
