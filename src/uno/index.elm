import List exposing (length, take, drop, isEmpty)
import Tuple exposing (first, second)

import Html exposing (Html, div, text, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random exposing (Seed)

import Cards exposing (Card, createDeck)
import Randy exposing (shuffle)
import Elves exposing (hl, tl)

-- Model

type alias Model = {
  deck: List (Card),
  seed: Random.Seed,
  p1: List (Card),
  p2: List (Card),
  discard: List (Card),
  playing: String
}

init : (Model, Cmd Msg)
init =
  ((Model createDeck (Random.initialSeed 1982211) [] [] [] "none"), Cmd.none)

-- Update

distro: List (Card) -> (List (Card), List (Card)) -> (List (Card), List (Card))
distro cards results =
  if (List.length cards) < 2 then results
  else distro (List.drop 2 cards) ((first results) ++ (hl cards), (hl (tl cards)) ++ (second results))

distribute: Model -> (List (Card), List (Card)) -> Model
distribute model hands = { model |
  p1 = first hands,
  p2 = second hands,
  deck = List.drop 14 model.deck,
  playing = "p1"
  }

type Msg = Start | Shuffle | Distribute

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Shuffle -> ({ model | deck = shuffle model.deck model.seed}, Cmd.none)
    Distribute -> ( (distribute model (distro (List.take 14 model.deck) ([], []) )), Cmd.none  )
    _ -> (model, Cmd.none)


-- subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- View

drawCard : Card -> Html Msg
drawCard card = div [ style [("display", "inline-block"), ("width", "50px"), ("height", "50px"), ("text-align", "center"), ("margin", "2px"), ("border-width", "5px"),("border-style", "solid"), ("border-color", if card.color == "wild" then "black" else card.color)]] [ text card.denom ]

view : Model -> Html Msg
view model = div [] [
    button [onClick Shuffle] [ text "Shuffle" ],
    button [onClick Distribute] [ text "Distribute" ],
    div [ id "p1", style [("border", "1px solid #000")] ] (List.map drawCard model.p1),
    div [ id "p2", style [("border", "1px solid #000")] ] (List.map drawCard model.p2),
    div [] (List.map drawCard model.deck)
    -- [ text ("DECK " ++ (toString (List.length model.deck))) ]
  ]


main =
  Html.program
    {
    init = init,
    view = view,
    update = update,
    subscriptions = subscriptions
    }
