module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Canvas exposing (..)
import Color exposing (..)
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (class, disabled, src, style)
import Html.Events exposing (onClick)
import Random



---- MODEL ----


type alias RandomSpread =
    { canvas : Canvas
    , horizontalBorders : List Int
    , verticalBorders : List Int
    , blocks : List Block
    }


type alias Model =
    { canvas : Canvas
    , height : Height
    , width : Width
    , horizontalBorders : List Int
    , verticalBorders : List Int
    , blocks : List Block
    }


initDimension : Int
initDimension =
    40


init : ( Model, Cmd Msg )
init =
    ( { canvas = []
      , height = initDimension
      , width = initDimension
      , horizontalBorders = []
      , verticalBorders = []
      , blocks = []
      }
    , newSpread initDimension initDimension
    )



---- UPDATE ----


type Msg
    = NoOp
    | NewSpread RandomSpread
    | NewCanvas Canvas
    | ChangeHeight Height
    | ChangeWidth Width
    | SetSquare
    | Scramble


zeroTo : Int -> Random.Generator Int
zeroTo max =
    Random.int 0 max


randomPrimaryColor : Random.Generator Color
randomPrimaryColor =
    Random.uniform Blue [ Red, Yellow ]


randomRow : Width -> Random.Generator (List Color)
randomRow w =
    Random.list w
        (Random.weighted ( 40, White )
            [ ( 1, Black )
            , ( 10, Red )
            , ( 10, Yellow )
            , ( 10, Blue )
            ]
        )


randomCanvas : Height -> Width -> Random.Generator Canvas
randomCanvas h w =
    Random.list h <| randomRow w


randomInt : Int -> Random.Generator (List Int)
randomInt d =
    Random.int 1 5
        |> Random.andThen
            (\len -> Random.list len (zeroTo d))


randomBlocks : Int -> Random.Generator (List Block)
randomBlocks d =
    let
        toBlock xy w h c =
            ( xy, { width = w, height = h }, c )

        fullRange =
            zeroTo d

        halfRange =
            zeroTo (d // 2)
    in
    Random.int 1 10
        |> Random.andThen
            (\len ->
                Random.list len
                    (Random.map4 toBlock
                        (Random.pair fullRange fullRange)
                        halfRange
                        halfRange
                        randomPrimaryColor
                    )
            )


newCanvas : Height -> Width -> Cmd Msg
newCanvas h w =
    Random.generate NewCanvas (randomCanvas h w)


newSpread : Width -> Height -> Cmd Msg
newSpread w h =
    Random.generate NewSpread
        (Random.map4 RandomSpread
            (randomCanvas w h)
            (randomInt h)
            (randomInt w)
            (randomBlocks (List.maximum [ w, h ] |> Maybe.withDefault 0))
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewSpread { canvas, horizontalBorders, verticalBorders, blocks } ->
            ( { model
                | canvas = canvas
                , horizontalBorders = horizontalBorders
                , verticalBorders = verticalBorders
                , blocks = blocks
              }
            , Cmd.none
            )

        NewCanvas canvas ->
            ( { model | canvas = canvas }, Cmd.none )

        ChangeHeight h ->
            let
                newH =
                    max 10 (model.height + h)
            in
            ( { model | height = newH }, newCanvas newH model.width )

        ChangeWidth w ->
            let
                newW =
                    max 10 (model.width + w)
            in
            ( { model | width = newW }, newCanvas model.height newW )

        SetSquare ->
            let
                smaller =
                    min model.width model.height
            in
            ( { model | width = smaller, height = smaller }
            , newSpread smaller smaller
            )

        Scramble ->
            ( model, newSpread model.width model.height )

        _ ->
            ( model, Cmd.none )



---- VIEW ---


makeCell : Color -> Html Msg
makeCell c =
    div
        [ class "Cell"
        , style "background-color" (colorToString c)
        ]
        []


makeRow : List Color -> Html Msg
makeRow =
    List.map makeCell >> div [ class "Row" ]


makeBlocks : List Block -> Canvas -> Canvas
makeBlocks bs c =
    List.foldl (<|) c (List.map withBlock bs)


makeAdjustors : List ( List (Html.Attribute msg), List (Html msg) ) -> List (Html msg)
makeAdjustors =
    let
        colors =
            [ ( Blue, White ), ( Red, White ), ( Yellow, Black ) ]
    in
    indexed
        >> List.map
            (\( i, data ) ->
                case data of
                    ( [], [] ) ->
                        text ""

                    ( attrs, v ) ->
                        let
                            ( bg, color ) =
                                colors
                                    |> List.drop (modBy (List.length colors) i)
                                    |> List.head
                                    |> Maybe.withDefault ( White, Black )
                        in
                        button
                            (attrs
                                ++ [ style "color" (colorToString color)
                                   , style "background-color" (colorToString bg)
                                   , class "Adjustors__adjustor"
                                   ]
                            )
                            v
            )


view : Model -> Html Msg
view model =
    div [ class "Main" ]
        [ h1 [] [ text "MondriElm!" ]
        , div [ class "Adjustors" ]
            (makeAdjustors
                [ ( [ onClick Scramble ], [ text "Scramble" ] )
                , ( [ onClick <| ChangeHeight 5 ], [ text "Height +" ] )
                , ( [ onClick <| ChangeHeight -5 ], [ text "Height -" ] )
                , ( [ onClick <| ChangeWidth 5 ], [ text "Width +" ] )
                , ( [ onClick <| ChangeWidth -5 ], [ text "Width -" ] )
                , ( [ onClick <| SetSquare
                    , disabled <| model.width == model.height
                    ]
                  , [ text "Make Square" ]
                  )
                ]
            )
        , div [ class "Frame__outer" ]
            [ div [ class "Frame" ] <|
                List.map makeRow <|
                    withVerticalBorders model.verticalBorders Black <|
                        withHorizontalBorders model.horizontalBorders Black <|
                            makeBlocks model.blocks <|
                                model.canvas
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
