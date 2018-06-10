import Html exposing (Html, div, text, input, select, option)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

import Models exposing (Unit(..), unitStringify, readUnitFromString)

main =
  Html.beginnerProgram
    { model = model
    , view = view,
    update = update
    }

-- Model

type alias Model = {
  initUnit: Unit,
  initQty: Float,
  unitsTypes : List (String)
}
model : Model
model = {
  initUnit = Cup,
  initQty = 1.0,
  unitsTypes = (List.map unitStringify [X, Tsp, Tbsp, Floz, Cup, Pt, Qt, Gl])
  }

-- update

type Msg = UpdateUnit String | UpdateQty String

update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateQty value ->
      { model | initQty = Result.withDefault 0 (String.toFloat value) }
    UpdateUnit value ->
      { model | initUnit = (readUnitFromString value) }

-- View

unitOption : (String) -> Html Msg
unitOption unit = option [ value unit ] [ text unit ]

unitSelector : List (String) -> Html Msg
unitSelector opts =
  select [ onInput UpdateUnit] (List.map unitOption opts)


view : Model -> Html Msg
view model =
  div []
    [
    input [
      value (toString model.initQty),
      onInput UpdateQty
    ] [],
    unitSelector model.unitsTypes
    ]
