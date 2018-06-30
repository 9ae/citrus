import List exposing (take, drop, head, tail)
import Tuple exposing (first, second)

import Html exposing (Html, div, text, button, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random exposing (Seed)

import Cards exposing (Card, createDeck, divideDeck, filterActionCards, getFirstNumeric,    validateAdjacentCards, moveNCards, validateFirstCard, Action, parseAction, isAction)
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
  drawCounter: Int,
  currentColor: String
}

init : (GameState, Cmd Msg)
init =
  ((GameState createDeck (Random.initialSeed 238318) [] [] [] "none" "" [] 0 ""), Cmd.none)

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
      deck = (second res),
      currentColor = card.color
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
    validateFirstCard (te model.discard) card
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

applyAction1: Card -> GameState -> GameState
applyAction1 card model =
  case card.denom of
    "skip" -> { model |
      discard = model.discard ++ [card],
      playerQueue = []
    }
    "rev" -> { model |
      discard = model.discard ++ [card],
      playerQueue = []
    }
    "draw2" -> { model |
      discard = model.discard ++ [card],
      playerQueue = [],
      playing = togglePlaying model.playing,
      drawCounter = model.drawCounter + 2
    }
    "draw4" -> { model |
      discard = model.discard ++ [card],
      playerQueue = [],
      drawCounter = model.drawCounter + 4,
      currentColor = "wild"
    }
    "wild" -> { model |
      discard = model.discard ++ [card],
      playerQueue = [],
      currentColor = "wild"
    }
    _ -> model

applyAction: Maybe Card -> GameState -> GameState
applyAction mCard model =
  case mCard of
    Just card -> applyAction1 card model
    Nothing -> model

updateColor: Maybe Card -> String -> String
updateColor mc existingColor =
  case mc of
    Just card -> card.color
    Nothing -> existingColor

playHand: GameState -> GameState
playHand model =
  if (List.length model.playerQueue) == 1 && isAction (head model.playerQueue) then
    applyAction (head model.playerQueue) model
  else
    { model |
      discard = model.discard ++ model.playerQueue,
      playerQueue = [],
      playing = togglePlaying model.playing,
      currentColor = updateColor (te model.playerQueue) model.currentColor
    }

reshuffle: GameState -> GameState
reshuffle model =
  let inverse = (List.reverse model.discard)
  in { model |
    deck = shuffle (model.deck ++ (drop 1 inverse)) model.seed,
    discard = (take 1 inverse)
  }

drawNCards: Int -> GameState -> GameState
drawNCards n model =
  if (List.length model.deck) < n then drawNCards n (reshuffle model)
  else
    { model |
      p1 = if model.playing == "p1" then (moveNCards n model.deck model.p1) else model.p1,
      p2 = if model.playing == "p2" then (moveNCards n model.deck model.p2) else model.p2,
      deck = List.drop n model.deck,
      drawCounter = 0
    }

-- Update

type Msg = Start | Shuffle | Play Int Card | PlayHand | Draw Int | ChangeColor String

update : Msg -> GameState -> (GameState, Cmd Msg)
update msg model =
  case msg of
    Shuffle -> ({ model | deck = shuffle model.deck model.seed}, Cmd.none)
    Start -> ( (startGame (distribute model 14)), Cmd.none  )
    Play index card -> ((playCard model index card), Cmd.none)
    PlayHand -> ((playHand model), Cmd.none)
    Draw n -> ((drawNCards (if n == 0 then 1 else n) model), Cmd.none)
    ChangeColor newColor -> ({ model |
        currentColor = newColor,
        playing = togglePlaying model.playing
      }, Cmd.none)
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

colorButtonView: String -> Html Msg
colorButtonView c = button [ onClick (ChangeColor c), style [("background-color", c)] ] [ text c ]

wildColorChangeView: String -> Html Msg
wildColorChangeView currentColor =
  div [ style [("display", if currentColor == "wild" then "block" else "none" )] ]
    (List.map colorButtonView ["red", "blue", "green", "yellow"])

view : GameState -> Html Msg
view model = div [] [
    button [onClick Shuffle] [ text "Shuffle" ],
    button [onClick Start, disabled (model.playing /= "none")] [ text "Start" ],
    button [onClick PlayHand, disabled (List.isEmpty model.playerQueue)] [ text "Play" ],
    p [ style [("min-height", "20px"), ("background-color", model.currentColor)] ] [ text model.message ],
    p [] [ text ("Active Player: " ++ model.playing) ],
    div [] (List.map cardView model.playerQueue),
    wildColorChangeView model.currentColor,
    div [ id "p1", style [("border", "4px solid purple")] ]
      (playerHandView model.p1 (validatePlayer model "p1")),
    div [ id "p2", style [("border", "4px solid orange")] ]
      (playerHandView model.p2 (validatePlayer model "p2")),
    button [onClick (Draw model.drawCounter)] [ text "Draw" ],
    div [ id "discard", style [("border", "1px solid gray")]] (List.map cardView (List.reverse model.discard)),
    div [] (List.map cardView model.deck)

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
