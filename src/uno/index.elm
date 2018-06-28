import Html exposing (Html, div, text, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Array exposing (Array, fromList, slice, append, isEmpty, get, length)
import Tuple exposing (first, second)

import Cards exposing (Card, createDeck)
import Random

-- Model

type alias Model = {
  deck: List (Card),
  seed: Random.Seed,
  p1: List (Card),
  p2: List (Card)
}

init : (Model, Cmd Msg)
init =
  ((Model createDeck (Random.initialSeed 1982211) [] []), Cmd.none)

-- Update

shake: Random.Seed -> Int -> Int -> (Int, Random.Seed)
shake seed i j = Random.step (Random.int i j) seed

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
    _ -> cutDeck (newDeck cards (first rs)) (it - 1) (shake (second rs) 1 ((length cards) - 2))

shuffle: List (Card) -> Random.Seed -> List (Card)
shuffle cards seed = Array.toList (cutDeck (Array.fromList cards) 500 (shake seed 1 ((List.length cards) - 2)))

-- distro: List (Card) -> Int -> (List (Card), List (Card)) -> (List (Card), List (Card))
-- distro cards it results =
--   case it of
--     0 -> results
--     _ -> distro (List.drop 2 cards) (it - 1) ((List.head cards) :: (first results), (List.head (List.tail cards)) :: (second results) )

type Msg = Start | Shuffle | Distribute

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Shuffle -> ({ model | deck = shuffle model.deck model.seed}, Cmd.none)
    Distribute -> ({
      model | p1 = (List.take 7 model.deck), p2 = (List.take 7 (List.drop 7 model.deck)), deck = (List.drop 14 model.deck)
    }, Cmd.none)
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
    button [onClick Distribute] [ text "Distribute" ],
    div [ id "p1", style [("border", "1px solid #000")] ] (List.map drawCard model.p1),
    div [ id "p2", style [("border", "1px solid #000")] ] (List.map drawCard model.p2),
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
