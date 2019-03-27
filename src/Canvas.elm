module Canvas exposing (Canvas, withHorizontalBorders, withVerticalBorders)

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


withVerticalBorders : List Int -> Color -> Canvas -> Canvas
withVerticalBorders columns co =
    List.map
        (List.indexedMap Tuple.pair
            >> List.map
                (\( i, c ) ->
                    if List.member i columns then
                        co

                    else
                        c
                )
        )
