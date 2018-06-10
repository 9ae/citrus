import Html exposing (Html, div, text, select, option)
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
type Unit = Tsp | Tbsp | Floz | Cup | Pt | Qt | Gl
unitStringify: Unit -> String
unitStringify u =
  case u of
    Tsp -> "teaspoon"
    Tbsp -> "tablespoon"
    Floz -> "fluid ounce"
    Cup -> "cup"
    Pt -> "pint"
    Qt -> "quart"
    Gl -> "gallon"

type alias Measurement = {
  unit : Unit,
  qty : Float
}
measurementStringify: Measurement -> String
measurementStringify m = pluralize m.qty (unitStringify m.unit)

-- Model

type alias Model = {
  initMeasure: Measurement,
  unitsTypes : List (Unit, String)
}
model : Model
model = {
  initMeasure = { unit =  Cup, qty = 1.0 },
  unitsTypes = [(Tsp, "teaspoon"), (Tbsp, "tablespoon"), (Floz, "fluid ounce"), (Cup, "cup"), (Pt, "pint"), (Qt, "quart"), (Gl, "gallon")]
  }

-- update

update : msg -> Model -> Model
update msg model = model

-- View

unitOption : (Unit, String) -> Html msg
unitOption pair = option [] [text (Tuple.second pair)]

unitSelector : List (Unit, String) -> Html msg
unitSelector opts =
  select [] (List.map unitOption opts)


view : Model -> Html msg
view model =
  div []
    [ text ("Starting with " ++ (measurementStringify model.initMeasure) ),
    unitSelector model.unitsTypes
    ]
