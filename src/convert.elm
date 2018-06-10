import Html exposing (Html, div, text, input, select, option)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

main =
  Html.beginnerProgram
    { model = model
    , view = view,
    update = update
    }

-- General functions
pluralize: Float -> String -> String
pluralize n str =
  if n > 1 then ((toString n) ++ " " ++ str ++ "s") else ((toString n) ++ " " ++ str)

tbsp2tsp n = n * 3
floz2tbsp n = n * 2
cup2floz n = n * 8
pt2cup n = n * 2
qt2pt n = n * 2
gl2qt n = n * 4

-- Data structures
type Unit = X | Tsp | Tbsp | Floz | Cup | Pt | Qt | Gl
unitStringify: Unit -> String
unitStringify u =
  case u of
    X -> " "
    Tsp -> "teaspoon"
    Tbsp -> "tablespoon"
    Floz -> "fluid ounce"
    Cup -> "cup"
    Pt -> "pint"
    Qt -> "quart"
    Gl -> "gallon"

readUnitFromString : String -> Unit
readUnitFromString str =
  case str of
    "teaspoon" -> Tsp
    "tablespoon" -> Tbsp
    "fluid ounce" -> Floz
    "cup" -> Cup
    "pint" -> Pt
    "quart" -> Qt
    "gallon" -> Gl
    _ -> X

type alias Measurement = {
  unit : Unit,
  qty : Float
}
measurementStringify: Measurement -> String
measurementStringify m = pluralize m.qty (unitStringify m.unit)

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
