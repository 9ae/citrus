import Html exposing (Html, div, text, input, select, option)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Array exposing (Array)

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
  unitsTypes : Array (String)
}
model : Model
model = {
  initUnit = Cup,
  initQty = 1.0,
  unitsTypes = Array.fromList (List.map unitStringify [Tsp, Tbsp, Floz, Cup, Pt, Qt, Gl])
  }

-- update

type Msg = UpdateUnit String | UpdateQty String | UpdateTargetUnit

update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateQty value ->
      { model | initQty = Result.withDefault 0 (String.toFloat value) }
    UpdateUnit value ->
      { model | initUnit = readUnitFromString (Maybe.withDefault "" (Array.get ( Result.withDefault 0 (String.toInt value)))) model.unitsTypes }

-- View

unitOption : Int -> String -> Html Msg
unitOption index unit = option [ value (toString index) ] [ text unit ]

unitSelector : Array (String) -> Html Msg
unitSelector opts =
  select [ onInput UpdateUnit] (Array.toList (Array.indexedMap unitOption opts))


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
