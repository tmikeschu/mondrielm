module Color exposing (Color(..), colorToString)


type Color
    = Blue
    | Red
    | Yellow
    | Black
    | White


colorToString : Color -> String
colorToString c =
    case c of
        Blue ->
            "#3963BA"

        Red ->
            "#B41907"

        Yellow ->
            "#EDB023"

        Black ->
            "#000000"

        White ->
            "#ffffff"
