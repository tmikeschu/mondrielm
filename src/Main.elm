module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Canvas exposing (..)
import Color exposing (..)
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick)
import Random



---- MODEL ----


type alias RandomSpread =
    { canvas : Canvas
    , horizontalBorders : List Int
    , verticalBorders : List Int
    }


type alias Model =
    { canvas : Canvas
    , height : Height
    , width : Width
    , horizontalBorders : List Int
    , verticalBorders : List Int
    }


type alias Height =
    Int


type alias Width =
    Int


initDimension : Int
initDimension =
    50


init : ( Model, Cmd Msg )
init =
    ( { canvas = []
      , height = initDimension
      , width = initDimension
      , horizontalBorders = []
      , verticalBorders = []
      }
    , newSpread initDimension
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


randomIntList : Int -> Random.Generator (List Int)
randomIntList d =
    Random.int 1 5
        |> Random.andThen
            (\len ->
                Random.list len
                    (Random.int 0
                        d
                    )
            )


newCanvas : Height -> Width -> Cmd Msg
newCanvas h w =
    Random.generate NewCanvas (randomCanvas h w)


newSpread : Int -> Cmd Msg
newSpread dimension =
    Random.generate NewSpread
        (Random.map3 RandomSpread (randomCanvas dimension dimension) (randomIntList dimension) (randomIntList dimension))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewSpread { canvas, horizontalBorders, verticalBorders } ->
            ( { model
                | canvas = canvas
                , horizontalBorders = horizontalBorders
                , verticalBorders = verticalBorders
              }
            , Cmd.none
            )

        NewCanvas canvas ->
            ( { model | canvas = canvas }, Cmd.none )

        ChangeHeight h ->
            ( { model | height = model.height + h }, newCanvas (model.height + h) model.width )

        ChangeWidth w ->
            ( { model | width = model.width + w }, newCanvas model.height (model.width + w) )

        SetSquare ->
            let
                smaller =
                    min model.width model.height
            in
            ( { model | width = smaller, height = smaller }
            , newCanvas smaller smaller
            )

        Scramble ->
            let
                smaller =
                    min model.width model.height
            in
            ( model, newSpread smaller )

        _ ->
            ( model, Cmd.none )



---- VIEW ---


makeCell : Color -> Html Msg
makeCell c =
    div
        [ class "Cell"
        , style "background-color" (colorToString c)
        , style "width" "1rem"
        , style "height" "1rem"
        ]
        []


makeRow : List Color -> Html Msg
makeRow =
    List.map makeCell >> div [ class "Row" ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "MondriElm!" ]
        , div [ class "Adjustors" ]
            [ button [ onClick <| Scramble ] [ text "Scramble" ]
            , button [ onClick <| ChangeHeight 5 ] [ text "Height +" ]
            , button [ onClick <| ChangeHeight -5 ] [ text "Height -" ]
            , button [ onClick <| ChangeWidth 5 ] [ text "Width +" ]
            , button [ onClick <| ChangeWidth -5 ] [ text "Width -" ]
            , button [ onClick <| SetSquare ] [ text "Make Square" ]
            ]
        , div [ class "Frame" ] <|
            List.map makeRow <|
                withVerticalBorders model.verticalBorders Black <|
                    withHorizontalBorders model.horizontalBorders Black <|
                        withBox { height = 15, width = 10 } ( 10, 40 ) Yellow <|
                            withBox { height = 10, width = 4 } ( 3, 10 ) Red <|
                                model.canvas
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
