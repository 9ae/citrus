import List exposing (length, take, drop, isEmpty)
import Tuple exposing (first, second)

import Html exposing (Html, div, text, button, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random exposing (Seed)

import Cards exposing (Card, createDeck, divideDeck, filterActionCards, getFirstNumeric,    validateAdjacentCards, moveNCards)
import Randy exposing (shuffle)
import Elves exposing (popn, te)

-- GameState

type alias GameState = {
  deck: List (Card),
  seed: Random.Seed,
  p1: List (Card),
  p2: List (Card),
  discard: List (Card),
  playing: String,
  message: String,
  playerQueue: List (Card),
  actionQueue: List (Card)
}

init : (GameState, Cmd Msg)
init =
  ((GameState createDeck (Random.initialSeed 1982211) [] [] [] "none" "" [] []), Cmd.none)

-- Game state methods

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

validatePlayer: GameState -> String -> Bool
validatePlayer model player = player == model.playing

validatePlay: GameState -> Card -> Bool
validatePlay model card =
  if List.isEmpty model.playerQueue then
    validateAdjacentCards (te model.discard) card
  else
    validateAdjacentCards (te model.playerQueue) card

applyPlay: GameState -> Int -> Card -> GameState
applyPlay model index card = { model |
    p1 = if model.playing == "p1" then popn index model.p1 else model.p1,
    p2 = if model.playing == "p2" then popn index model.p2 else model.p2,
    playerQueue = model.playerQueue ++ [card]
  }

playCard: GameState -> Int -> Card -> GameState
playCard model index card = if validatePlay model card then applyPlay model index card else model

togglePlaying: String -> String
togglePlaying playing = if playing == "p1" then "p2" else "p1"

playHand: GameState -> GameState
playHand model = { model |
    discard = model.discard ++ model.playerQueue,
    playerQueue = [],
    playing = togglePlaying model.playing,
    actionQueue = model.actionQueue ++ (filterActionCards model.playerQueue)
  }

drawNCards: Int -> GameState -> GameState
drawNCards n model = { model |
    p1 = if model.playing == "p1" then (moveNCards n model.deck model.p1) else model.p1,
    p2 = if model.playing == "p2" then (moveNCards n model.deck model.p2) else model.p2,
    deck = List.drop n model.deck
  }

-- Update

type Msg = Start | Shuffle | Play Int Card | PlayHand | Draw Int

update : Msg -> GameState -> (GameState, Cmd Msg)
update msg model =
  case msg of
    Shuffle -> ({ model | deck = shuffle model.deck model.seed}, Cmd.none)
    Start -> ( (startGame (distribute model 14)), Cmd.none  )
    Play index card -> ((playCard model index card), Cmd.none)
    PlayHand -> ((playHand model), Cmd.none)
    Draw n -> ((drawNCards n model), Cmd.none)
    -- _ -> (model, Cmd.none)


-- subscriptions

subscriptions : GameState -> Sub Msg
subscriptions model =
  Sub.none


-- View

cardView : Card -> Html Msg
cardView card = div [ style [("display", "inline-block"), ("width", "50px"), ("height", "50px"), ("text-align", "center"), ("margin", "2px"), ("border-width", "5px"),("border-style", "solid"), ("border-color", if card.color == "wild" then "black" else card.color)]] [ text card.denom ]

playerCardView: Bool -> Int -> Card -> Html Msg
playerCardView isTurn index card = button [
  onClick (Play index card),
  disabled (not isTurn)
  ] [(cardView card)]

playerHandView: List (Card) -> Bool -> List (Html Msg)
playerHandView hand isTurn = List.indexedMap (playerCardView isTurn) hand

view : GameState -> Html Msg
view model = div [] [
    button [onClick Shuffle] [ text "Shuffle" ],
    button [onClick Start, disabled (model.playing /= "none")] [ text "Start" ],
    button [onClick PlayHand, disabled (List.isEmpty model.playerQueue)] [ text "Play" ],
    p [] [ text model.message ],
    p [] [ text ("Active Player: " ++ model.playing) ],
    div [] (List.map cardView model.playerQueue),
    div [ style [("border", "1px solid red")] ] (List.map cardView model.actionQueue),
    div [ id "p1", style [("border", "1px solid purple")] ]
      (playerHandView model.p1 (validatePlayer model "p1")),
    div [ id "p2", style [("border", "1px solid orange")] ]
      (playerHandView model.p2 (validatePlayer model "p2")),
    button [onClick (Draw 1)] [ text "Draw" ],
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
