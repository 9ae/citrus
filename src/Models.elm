module Models exposing (Unit(..), unitStringify, readUnitFromString)

pluralize: Float -> String -> String
pluralize n str =
  if n > 1 then ((toString n) ++ " " ++ str ++ "s") else ((toString n) ++ " " ++ str)

tbsp2tsp n = n * 3
floz2tbsp n = n * 2
cup2floz n = n * 8
pt2cup n = n * 2
qt2pt n = n * 2
gl2qt n = n * 4

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
