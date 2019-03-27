module Canvas exposing (Canvas, withHorizontalBorders)

import Color exposing (Color)


type alias Canvas =
    List (List Color)


withHorizontalBorders : List Int -> Color -> Canvas -> Canvas
withHorizontalBorders rows co =
    List.indexedMap Tuple.pair
        >> List.map
            (\( i, r ) ->
                if List.member i rows then
                    List.map (always co) r

                else
                    r
            )
