import List exposing (length, take, drop, isEmpty)
import Tuple exposing (first, second)

import Html exposing (Html, div, text, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random exposing (Seed)

import Cards exposing (Card, createDeck, divideDeck, getFirstNumeric)
import Randy exposing (shuffle)
import Elves exposing (nl)

-- GameState

type alias GameState = {
  deck: List (Card),
  seed: Random.Seed,
  p1: List (Card),
  p2: List (Card),
  discard: List (Card),
  playing: String
}

init : (GameState, Cmd Msg)
init =
  ((GameState createDeck (Random.initialSeed 1982211) [] [] [] "none"), Cmd.none)

-- Update

distribute1: GameState -> Int -> (List (Card), List (Card)) -> GameState
distribute1 model n hands = { model |
  p1 = first hands,
  p2 = second hands,
  deck = List.drop n model.deck
  }

distribute: GameState -> Int -> GameState
distribute model n = distribute1 model n (divideDeck (List.take n model.deck) ([], []) )

drawStarterCard: (Maybe Card, List(Card)) -> GameState -> GameState
drawStarterCard res model =
  case (first res) of
    Just card -> { model |
      playing = "p1",
      discard = [card],
      deck = (second res)
    }
    Nothing -> model
    -- Should handle this error later

startGame: GameState -> GameState
startGame model = drawStarterCard (getFirstNumeric model.deck) model

type Msg = Start | Shuffle | Draw

update : Msg -> GameState -> (GameState, Cmd Msg)
update msg model =
  case msg of
    Shuffle -> ({ model | deck = shuffle model.deck model.seed}, Cmd.none)
    Start -> ( (startGame (distribute model 14)), Cmd.none  )
    _ -> (model, Cmd.none)


-- subscriptions

subscriptions : GameState -> Sub Msg
subscriptions model =
  Sub.none


-- View

cardView : Card -> Html Msg
cardView card = div [ style [("display", "inline-block"), ("width", "50px"), ("height", "50px"), ("text-align", "center"), ("margin", "2px"), ("border-width", "5px"),("border-style", "solid"), ("border-color", if card.color == "wild" then "black" else card.color)]] [ text card.denom ]

view : GameState -> Html Msg
view model = div [] [
    button [onClick Shuffle] [ text "Shuffle" ],
    button [onClick Start, disabled (model.playing /= "none")] [ text "Start" ],
    div [ id "p1", style [("border", "1px solid purple")] ] (List.map cardView model.p1),
    div [ id "p2", style [("border", "1px solid orange")] ] (List.map cardView model.p2),
    div [] (List.map cardView model.deck),
    div [ id "discard", style [("border", "1px solid gray")]] (List.map cardView model.discard)
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
