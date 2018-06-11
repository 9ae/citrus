import Html exposing (Html, div, text)
import Html.Attributes exposing (..)

import Cards exposing (Card, createDeck)

-- Model

type alias Model = {
  deck: List (Card)
}
model : Model
model = {
    deck = createDeck
  }

-- Update

type Msg = Start | Shuffle

update : Msg -> Model -> Model
update msg model = model

-- View

drawCard : Card -> Html Msg
drawCard card = div [ style [("display", "inline-block"), ("width", "50px"), ("height", "50px"), ("text-align", "center"), ("margin", "2px"), ("border-width", "5px"),("border-style", "solid"), ("border-color", if card.color == "wild" then "black" else card.color)] ] [ text card.denom ]

view : Model -> Html Msg
view model = div [] [
    div [class "cards-list"] (List.map drawCard model.deck)
  ]


main =
  Html.beginnerProgram
    { model = model
    , view = view,
    update = update
    }
