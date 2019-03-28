module Canvas exposing
    ( Canvas
    , getNeighbors
    , withBox
    , withHorizontalBorders
    , withVerticalBorders
    )

import Color exposing (Color)
import Set


type alias Canvas =
    List (List Color)


withHorizontalBorders : List Int -> Color -> Canvas -> Canvas
withHorizontalBorders rows co =
    indexed
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
        (indexed
            >> List.map
                (\( i, c ) ->
                    if List.member i columns then
                        co

                    else
                        c
                )
        )


type alias Dimension =
    { height : Int, width : Int }


type alias Coord =
    ( Int, Int )


getNeighbors : Dimension -> Coord -> List Coord
getNeighbors { height, width } ( x, y ) =
    let
        xs =
            List.range x (x + width - 1)

        ys =
            List.range y (y + height - 1)
    in
    List.concatMap
        (\x2 -> List.map (\y2 -> ( x2, y2 )) ys)
        xs



-- coord is top left oriented


withBox : Dimension -> Coord -> Color -> Canvas -> Canvas
withBox d coor col ca =
    ca
        |> indexed
        |> List.map
            (\( i, row ) ->
                row
                    |> indexed
                    |> List.map
                        (\( j, color ) ->
                            if List.member ( i, j ) (getNeighbors d coor) then
                                col

                            else
                                color
                        )
            )


indexed : List a -> List ( Int, a )
indexed =
    List.indexedMap Tuple.pair
